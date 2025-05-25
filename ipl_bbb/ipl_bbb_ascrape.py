import sys
import asyncio
import aiohttp
import aiofiles
import re
import json
import os
import logging
from asyncio import Semaphore
import time
from datetime import datetime

# Setup logging
log_dir = "logs"
os.makedirs(log_dir, exist_ok=True)
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
log_file = f"{log_dir}/ipl_bbb_scrape_{timestamp}.log"

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)

# Global dictionary to cache BBB IDs for matches.
matchBBBID = {}
missing_key_count = 0
header = [
    "batting_team",#1
    # "batsman_id",#2
    # "batsman_name",#3
    "batsman_is_rhb",#4
    "non_striker_id",#5
    "non_striker_name",#6
    "non_striker_is_rhb",#7
    "bowling_team",#8
    # "bowler_id",#9
    # "bowler_name",#10
    "bowler_is_rhb",#11
    "delivery_type",#12
    "innings",#13
    "ball",#14
    "over",#15
    "shot_is_attacked",#16
    "shot_is_played",#17
    # "shot_type",#18
    # "bounce_above_stumps",#19
    "bounce_angle",#20
    "bounce_pos_x",#21
    "bounce_pos_y",#22
    "boucne_pos_z",#23
    "crease_pos_x",#24
    "crease_pos_y",#25
    "crease_pos_z",#26
    "deviation",#27
    "drop_angle",#28
    "hit_stumps",#29
    "impact_pos_x",#30
    "impact_pos_y",#31
    "impact_pos_z",#32
    "initial_angle",#33
    "landing_pos_x",#34
    "landing_pos_y",#35
    "landing_pos_z",#36
    "pbr",#37
    "react_time_to_crease",#38
    "react_time_to_intercept",#39
    "real_distance",#40
    "release_pos_x",#41
    "release_pos_y",#42
    "release_pos_z",#43
    "release_speed",#44
    "spin_rate",#45
    "stump_pos_x",#46
    "stump_pos_y",#47
    "stump_pos_z",#48
    "swing",#49
    "match_id",#50
    "batsman_id",
    "batsman_name",
    "bowler_id",
    "bowler_name",
    "ball_id",#51
    "is_single",#52
    "is_double",#53
    "is_three",#54
    "is_dot",#55
    "is_wide",#56
    "is_no_ball",#57
    "is_bye",#58
    "is_leg_bye",#59
    "is_four",#60
    "is_six",#61
    "is_wicket",#62
    "wicket_type",#63
    "is_bowler_wicket",#64
    "ball_type",#65
    "shot_type",#66
    "pitch_x",#67
    "pitch_y",#68
    "ball_line",#69
    "ball_length",#70
    "runs",#71
    "actual_runs",#72
    "extras",#73
    "ball_runs",#74
    "is_bouncer",#75
    "is_free_hit",#76
    "innings_no",#77
    "ground",#78
    "date",#79
]


async def getSchedule(id, session):
    url = f"https://ipl-stats-sports-mechanic.s3.ap-south-1.amazonaws.com/ipl/feeds/{id}-matchschedule.js?MatchSchedule=_jqjsp&_1716223606780="
    async with session.get(url) as resp:
        content = await resp.read()
    resp_text = str(content)
    matchArray = re.findall(r'MatchSchedule\({"Matchsummary"\s?:(.*)}\);', resp_text)[0]
    sanitizedArray = re.sub(r'"PreMatchCommentary":.*?"MatchRow"', '"MatchRow"', matchArray)
    matches = json.loads(sanitizedArray)
    return matches


def getMatchURL(year, id):
    return f"https://www.iplt20.com/match/{year}/{id}"


async def getBbbID(year, id, session):
    url = getMatchURL(str(year).strip(), str(id).strip())
    async with session.get(url) as resp:
        content = await resp.read()
    content_str = str(content)
    bbbURL = re.findall(r'https:\/\/polls\.iplt20\.com\/\?entity_matchId=.*?"', content_str)
    bbbID = 0
    if bbbURL:
        bbbID = re.findall(r";matchId=(.*?)&", bbbURL[0])[0]
    return int(bbbID)


async def getBalls(id, inns, session):
    url = f"https://ipl-stats-sports-mechanic.s3.ap-south-1.amazonaws.com/ipl/feeds/{id}-Innings{inns}.js"
    async with session.get(url) as resp:
        content = await resp.read()
    content_str = str(content)
    pattern = r'onScoring\({"Innings' + str(inns) + r'"\s?:(.*)}\);'
    balls = re.findall(pattern, content_str)
    if not balls:
        return []
    sanitizedBalls = re.sub(r'"CommentStrikers".*?"Day"', '"Day"', balls[0])
    return json.loads(sanitizedBalls)["OverHistory"]


async def getBallsByMatch(id, session):
    # Run both innings sequentially (alternatively, you can schedule them concurrently)
    balls1 = await getBalls(id, 1, session)
    balls2 = await getBalls(id, 2, session)
    return balls1 + balls2


async def getBBB(inns, over, ball, id, session):
    # Ensure all parameters are non-empty strings
    inns_str = str(inns) if inns else "0"
    over_str = str(over) if over else "0"
    ball_str = str(ball) if ball else "0"
    id_str = str(id) if id else "0"
    
    # Construct URL with proper parameters
    url = f"https://post-feeds.s3.ap-south-1.amazonaws.com/Delivery_{inns_str}_{over_str}_{ball_str}_{id_str}.json"
    
    try:
        async with session.get(url) as resp:
            content = await resp.read()
            content_str = str(content)
            
        # Check for XML Access Denied response
        if "<?xml" in content_str and "AccessDenied" in content_str:
            logger.warning(f"Access Denied for URL: {url}")
            return [None]*43
            
        # For debugging - print the raw content for some requests
        if len(content_str) < 10:  # If content is suspiciously small
            logger.warning(f"Empty/small response for URL: {url}")
            logger.debug(f"Content: {content}")
            return [None]*43
            
        data_match = re.findall(r"b'(.*)'", content_str)
        if not data_match:
            # Try alternate pattern if original pattern fails
            data_match = re.findall(r'b"(.*)"', content_str)
            if not data_match:
                logger.warning(f"No data match pattern for URL: {url}")
                return [None]*43
                
        data_extracted = data_match[0]
        if not data_extracted:
            logger.warning(f"Empty data extracted for URL: {url}")
            return [None]*43
            
        sanitizedData = re.sub(r',"trajectoryData".*?}', "}", data_extracted)
        
        try:
            bbb = json.loads(sanitizedData)
            if not bbb:
                return [None]*43
        except json.JSONDecodeError as e:
            logger.error(f"JSON decode error for URL: {url}")
            logger.error(f"Error: {e}")
            logger.error(f"Content snippet: {sanitizedData[:100]}")
            return [None]*43
            
        return [
            bbb["match"]["battingTeam"]["name"],#1
            # bbb["match"]["battingTeam"]["batsman"]["id"],#2
            # bbb["match"]["battingTeam"]["batsman"]["name"],#3
            bbb["match"]["battingTeam"]["batsman"]["isRightHanded"],#4
            bbb["match"]["battingTeam"]["batsmanPartner"]["id"],#5
            bbb["match"]["battingTeam"]["batsmanPartner"]["name"],#6
            bbb["match"]["battingTeam"]["batsmanPartner"]["isRightHanded"],#7
            bbb["match"]["bowlingTeam"]["name"],#8
            # bbb["match"]["bowlingTeam"]["bowler"]["id"],#9
            # bbb["match"]["bowlingTeam"]["bowler"]["name"],#10
            bbb["match"]["bowlingTeam"]["bowler"]["isRightHanded"],#11
            bbb["match"]["delivery"]["deliveryType"],#12
            bbb["match"]["delivery"]["deliveryNumber"]["innings"],#13
            bbb["match"]["delivery"]["deliveryNumber"]["ball"],#14
            bbb["match"]["delivery"]["deliveryNumber"]["over"],#15
            bbb["match"]["delivery"]["shotInformation"]["shotAttacked"],#16
            bbb["match"]["delivery"]["shotInformation"]["shotPlayed"],#17
            # bbb["match"]["delivery"]["shotInformation"]["shotTypeAdditional"],#18
            # bbb["match"]["delivery"]["trajectory"]["bounceAboveStumps"],#19
            bbb["match"]["delivery"]["trajectory"]["bounceAngle"],#20
            bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["x"],#21
            bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["y"],#22
            bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["z"],#23
            bbb["match"]["delivery"]["trajectory"]["creasePosition"]["x"],#24
            bbb["match"]["delivery"]["trajectory"]["creasePosition"]["y"],#25
            bbb["match"]["delivery"]["trajectory"]["creasePosition"]["z"],#26
            bbb["match"]["delivery"]["trajectory"]["deviation"],#27
            bbb["match"]["delivery"]["trajectory"]["dropAngle"],#28
            bbb["match"]["delivery"]["trajectory"]["hitStumps"],#29
            bbb["match"]["delivery"]["trajectory"]["impactPosition"]["x"],#30
            bbb["match"]["delivery"]["trajectory"]["impactPosition"]["y"],#31
            bbb["match"]["delivery"]["trajectory"]["impactPosition"]["z"],#32
            bbb["match"]["delivery"]["trajectory"]["initialAngle"],#33
            bbb["match"]["delivery"]["trajectory"]["landingPosition"]["x"],#34
            bbb["match"]["delivery"]["trajectory"]["landingPosition"]["y"],#35
            bbb["match"]["delivery"]["trajectory"]["landingPosition"]["z"],#36
            bbb["match"]["delivery"]["trajectory"]["pbr"],#37
            bbb["match"]["delivery"]["trajectory"]["reactionTime(to crease)"],#38
            bbb["match"]["delivery"]["trajectory"]["reactionTime(to interception)"],#39
            bbb["match"]["delivery"]["trajectory"]["realDistance"],#40
            bbb["match"]["delivery"]["trajectory"]["releasePosition"]["x"],#41
            bbb["match"]["delivery"]["trajectory"]["releasePosition"]["y"],#42
            bbb["match"]["delivery"]["trajectory"]["releasePosition"]["z"],#43
            bbb["match"]["delivery"]["trajectory"]["releaseSpeed"],#44
            bbb["match"]["delivery"]["trajectory"]["spinRate"],#45
            bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["x"],#46
            bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["y"],#47
            bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["z"],#48
            bbb["match"]["delivery"]["trajectory"]["swing"],#49
        ]
    except Exception as e:
        logger.error(f"Error in getBBB for URL {url}: {str(e)}")
        return [None]*43


async def writeData(match, b, index, b_index, matches, balls, argYear, session, file_lock):
    global missing_key_count
    # Fix the extraction of the match year (assume format "YYYY-MM-DD")
    match_year = match["MatchDate"].split("-")[0]
    # Retrieve the BBB ID (cache if already seen)
    if match["MatchID"] in matchBBBID:
        bbbID = matchBBBID[match["MatchID"]]
    else:
        bbbID = await getBbbID(match_year, match["MatchID"], session)
        matchBBBID[match["MatchID"]] = bbbID

    # Make sure to provide defaults for required parameters
    innings_no = b.get("InningsNo", 0)
    over_no = b.get("OverNo", 0)
    actual_ball_no = b.get("ActualBallNo", 0)
    
    if actual_ball_no:
        bbb = await getBBB(innings_no, over_no, actual_ball_no, bbbID, session)
    else:
        logger.warning(f"ActualBallNo not found for match {b.get('MatchID', 'Unknown')}")
        bbb = [None] * 43

    # Check for missing key before extending
    if "IsBowlerWicket" not in b or "IsBouncer" not in b or "IsFreeHit" not in b:
        missing_key_count += 1
        logger.warning(f"IsBowlerWicket missing in match {b.get('MatchID', 'Unknown')}, innings {innings_no}, over {over_no}, ball {actual_ball_no}")
    
    bbb.extend([
        b.get("MatchID", ""),#50
        b.get('StrikerID', ""),
        b.get("BatsManName", ""),
        b.get('BowlerID',''),
        b.get('BowlerName',''),
        b.get("BallUniqueID", 0),#51
        b.get("IsOne", False),#52
        b.get("IsTwo", False),#53
        b.get("IsThree", False),#54
        b.get("IsDotball", False),#55
        b.get("IsWide", False),#56
        b.get("IsNoBall", False),#57
        b.get("IsBye", False),#58
        b.get("IsLegBye", False),#59
        b.get("IsFour", False),#60
        b.get("IsSix", False),#61
        b.get("IsWicket", False),#62
        b.get("WicketType", ""),#63
        b.get("IsBowlerWicket", False),#64
        b.get("BowlTypeName", ""),#65
        b.get("ShotType", ""),#66

        b.get("Xpitch", 0),#67
        b.get("Ypitch", 0),#68
        b.get("BOWLING_LINE_ID", 0),#69
        b.get("BOWLING_LENGTH_ID", 0),#70
        int(re.search(r'\d+', b["Runs"]).group()) if b.get("Runs") and re.search(r'\d+', b["Runs"]) else 0,#71
        b.get('ActualRuns', 0),#72
        b.get('Extras',0),#73
        b.get('BallRuns',0),#74
        b.get("IsBouncer", False),#75
        b.get("IsFreeHit", False),#76
        innings_no,#77
        match.get("GroundName", ""),#78
        match.get("MatchDate", ""),#79
    ])

    row = ",".join(map(str, bbb))
    logger.info(f"{index+1}/{len(matches)} - {b_index+1}/{len(balls)} done.")
    async with file_lock:
        async with aiofiles.open(f"./datasets/data{argYear}.csv", mode="a") as file:
            await file.write(row + "\n")


async def getData(matches, argYear, session):
    
    # Write the header into the file.
    async with aiofiles.open(f"./datasets/data{argYear}.csv", mode="w") as file:
        await file.write(",".join(header) + "\n")
        
    tasks = []
    file_lock = asyncio.Lock()
    # Create a semaphore to limit concurrent requests (adjust the number based on your needs)
    request_semaphore = Semaphore(200)  # Allow concurrent requests
    
    # For each match, get its balls and schedule tasks for each ball.
    for index, match in enumerate(matches):
        balls = await getBallsByMatch(match["MatchID"], session)
        for b_index, b in enumerate(balls):
            # Wrap the task with semaphore
            task = asyncio.create_task(
                process_ball(match, b, index, b_index, matches, balls, argYear, session, file_lock, request_semaphore)
            )
            tasks.append(task)
            
    await asyncio.gather(*tasks)


# New helper function to handle the semaphore
async def process_ball(match, b, index, b_index, matches, balls, argYear, session, file_lock, semaphore):
    async with semaphore:  # This ensures only N concurrent requests
        await writeData(match, b, index, b_index, matches, balls, argYear, session, file_lock)


async def main():
    start_time = time.time()

    # Mapping year to schedule id.
    schedule_ids = {
        "2022": 60,
        "2023": 107,
        "2024": 148,
        "2025": 203,
    }
    
    logger.info("Starting IPL BBB data scraping")
    
    for argYear, schedule_id in schedule_ids.items():    
        logger.info(f"Processing data for year {argYear}")
        
        async with aiohttp.ClientSession() as session:
            matches = await getSchedule(schedule_id, session)
            logger.info(f"Found {len(matches)} matches for year {argYear}")
            await getData(matches, argYear, session)
        
        year_end_time = time.time()
        logger.info(f"Finished processing year {argYear}. Time taken: {year_end_time - start_time:.2f} seconds")

    end_time = time.time()
    logger.info(f"Total time taken: {end_time - start_time:.2f} seconds")
    global missing_key_count
    logger.info(f"Missing key count: {missing_key_count}")
    
    
if __name__ == "__main__":
    asyncio.run(main())