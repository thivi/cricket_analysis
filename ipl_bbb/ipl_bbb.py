# -*- coding: utf-8 -*-
"""IPL_BBB.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1SvjkOOy3pHVweej4MRSqz9GJMMCPLSiB
"""

import sys
import threading
from time import sleep
import requests
import re
import json

argYear = sys.argv[1]


def getSchedule(id):
    resp = requests.get(
        f"https://ipl-stats-sports-mechanic.s3.ap-south-1.amazonaws.com/ipl/feeds/{id}-matchschedule.js?MatchSchedule=_jqjsp&_1716223606780="
    )
    resp = str(resp.content)
    matchArray = re.findall('MatchSchedule\({"Matchsummary"\s?:(.*)}\);', resp)[0]
    sanitizedArray = re.sub(
        '"PreMatchCommentary":.*?"MatchRow"', '"MatchRow"', matchArray
    )
    matches = json.loads(sanitizedArray)

    return matches


def getMatchURL(year, id):
    return f"https://www.iplt20.com/match/{year}/{id}"


def getBbbID(year, id):
    resp = requests.get(getMatchURL(str(year).strip(), str(id).strip()))
    bbbURL = re.findall(
        'https:\/\/polls\.iplt20\.com\/\?entity_matchId=.*?"', str(resp.content)
    )
    bbbID = 0
    if len(bbbURL) > 0:
        bbbID = re.findall(";matchId=(.*?)&", bbbURL[0])[0]

    return int(bbbID)


def getBalls(id, inns):
    resp = requests.get(
        f"https://ipl-stats-sports-mechanic.s3.ap-south-1.amazonaws.com/ipl/feeds/{id}-Innings{inns}.js"
    )
    balls = re.findall(
        'onScoring\({"Innings' + str(inns) + '"\s?:(.*)}\);', str(resp.content)
    )

    if len(balls) == 0:
        return []

    sanitizedBalls = re.sub('"CommentStrikers".*?"Day"', '"Day"', balls[0])

    return json.loads(sanitizedBalls)["OverHistory"]


def getBallsByMatch(id):
    balls = []
    balls.extend(getBalls(id, 1))
    balls.extend(getBalls(id, 2))

    return balls


def getBBB(inns, over, ball, id):
    bbbURL = f"https://polls.iplt20.com/widget/welcome/get_data?path=Delivery_{inns}_{over}_{ball}_{id}.json"

    resp = requests.get(bbbURL)
    data = re.findall("b'(.*)'", str(resp.content))[0]
    sanitizedData = re.sub(',"trajectoryData".*?}', "}", data)

    bbb = json.loads(sanitizedData)

    if bbb is None:
        return []

    return [
        bbb["match"]["battingTeam"]["name"],
        bbb["match"]["battingTeam"]["batsman"]["id"],
        bbb["match"]["battingTeam"]["batsman"]["name"],
        bbb["match"]["battingTeam"]["batsman"]["isRightHanded"],
        bbb["match"]["battingTeam"]["batsmanPartner"]["id"],
        bbb["match"]["battingTeam"]["batsmanPartner"]["name"],
        bbb["match"]["battingTeam"]["batsmanPartner"]["isRightHanded"],
        bbb["match"]["bowlingTeam"]["name"],
        bbb["match"]["bowlingTeam"]["bowler"]["id"],
        bbb["match"]["bowlingTeam"]["bowler"]["name"],
        bbb["match"]["bowlingTeam"]["bowler"]["isRightHanded"],
        bbb["match"]["delivery"]["deliveryType"],
        bbb["match"]["delivery"]["deliveryNumber"]["innings"],
        bbb["match"]["delivery"]["deliveryNumber"]["ball"],
        bbb["match"]["delivery"]["deliveryNumber"]["over"],
        bbb["match"]["delivery"]["shotInformation"]["shotAttacked"],
        bbb["match"]["delivery"]["shotInformation"]["shotPlayed"],
        bbb["match"]["delivery"]["shotInformation"]["shotTypeAdditional"],
        bbb["match"]["delivery"]["trajectory"]["bounceAboveStumps"],
        bbb["match"]["delivery"]["trajectory"]["bounceAngle"],
        bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["bouncePosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["creasePosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["creasePosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["creasePosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["deviation"],
        bbb["match"]["delivery"]["trajectory"]["dropAngle"],
        bbb["match"]["delivery"]["trajectory"]["hitStumps"],
        bbb["match"]["delivery"]["trajectory"]["impactPosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["impactPosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["impactPosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["initialAngle"],
        bbb["match"]["delivery"]["trajectory"]["landingPosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["landingPosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["landingPosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["pbr"],
        bbb["match"]["delivery"]["trajectory"]["reactionTime(to crease)"],
        bbb["match"]["delivery"]["trajectory"]["reactionTime(to interception)"],
        bbb["match"]["delivery"]["trajectory"]["realDistance"],
        bbb["match"]["delivery"]["trajectory"]["releasePosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["releasePosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["releasePosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["releaseSpeed"],
        bbb["match"]["delivery"]["trajectory"]["spinRate"],
        bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["x"],
        bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["y"],
        bbb["match"]["delivery"]["trajectory"]["stumpPosition"]["z"],
        bbb["match"]["delivery"]["trajectory"]["swing"],
    ]


header = [
    "batting_team",
    "batsman_id",
    "batsman_name",
    "batsman_is_rhb",
    "non_striker_id",
    "non_striker_name",
    "non_striker_is_rhb",
    "bowling_team",
    "bowler_id",
    "bowler_name",
    "bowler_is_rhb",
    "delivery_type",
    "innings",
    "ball",
    "over",
    "shot_is_attacked",
    "shot_is_played",
    "shot_type",
    "bounce_above_stumps",
    "bounce_angle",
    "bounce_pos_x",
    "bounce_pos_y",
    "boucne_pos_z",
    "crease_pos_x",
    "crease_pos_y",
    "crease_pos_z",
    "deviation",
    "drop_angle",
    "hit_stumps",
    "impact_pos_x",
    "impact_pos_y",
    "impact_pos_z",
    "initial_angle",
    "landing_pos_x",
    "landing_pos_y",
    "landing_pos_z",
    "pbr",
    "react_time_to_crease",
    "react_time_to_intercept",
    "real_distance",
    "release_pos_x",
    "release_pos_y",
    "release_pos_z",
    "release_speed",
    "spin_rate",
    "stump_pos_x",
    "stump_pos_y",
    "stump_pos_z",
    "swing",
    "match_id",
    "ball_id",
    "is_single",
    "is_double",
    "is_three",
    "is_dot",
    "is_wide",
    "is_no_ball",
    "is_bye",
    "is_leg_bye",
    "is_four",
    "is_six",
    "is_wicket",
    "wicket_type",
    "is_bowler_wicket",
    "ball_type",
    "shot_type",
    "pitch_x",
    "pitch_y",
    "ball_line",
    "ball_length",
    "runs",
    "is_bouncer",
    "is_free_hit",
    "innings_no",
    "ground",
    "date",
]

lock = threading.Lock()

matchBBBID = {}

def writeData(match, b, index, b_index, matches, balls):
    year = match["MatchDate"].split("-"[0])

    if match["MatchID"] in matchBBBID:
        bbbID = matchBBBID[match["MatchID"]]
    else:
        bbbID = getBbbID(year, match["MatchID"])
        matchBBBID[match["MatchID"]] = bbbID

    if "ActualBallNo" in b:
      bbb = getBBB(
          b["InningsNo"], b["OverNo"], b["ActualBallNo"], bbbID
      )
    else:
        bbb = []
        print(f"ActualBallNo not found for {b['MatchID']}")
    if len(bbb) > 0:
        bB = bbb[2]
    else:
        bB = ""
        print(f"bbb not found for {b['MatchID']},  {b['InningsNo']}, {b['OverNo']}, {b['ActualBallNo'] if 'ActualBallNo' in b else 'None'} {bbbID}")

    bat = b["BatsManName"] if "BatsManName" in b else ""

    if bB.lower() != bat.lower():
        print(
            f"{bB} - {bat}: {b['InningsNo']}, {b['OverNo']}, {b['ActualBallNo'] if 'ActualBallNo' in b else 'None'}, {bbbID}, {b['MatchID']}"
        )
    bbb.extend(
        [
            b["MatchID"],
            b["BallUniqueID"] if "BallUniqueID" in b else 0,
            b["IsOne"],
            b["IsTwo"],
            b["IsThree"],
            b["IsDotball"],
            b["IsWide"],
            b["IsNoBall"],
            b["IsBye"],
            b["IsLegBye"],
            b["IsFour"],
            b["IsSix"],
            b["IsWicket"],
            b["WicketType"],
            b["IsBowlerWicket"],
            b["BowlTypeName"],
            b["ShotType"],
            b["Xpitch"],
            b["Ypitch"],
            b["BOWLING_LINE_ID"] if "BOWLING_LINE_ID" in b else 0,
            b["BOWLING_LENGTH_ID"] if "BOWLING_LENGTH_ID" in b else 0,
            b["Runs"],
            b["IsBouncer"],
            b["IsFreeHit"],
            b["InningsNo"],
            match["GroundName"],
            match["MatchDate"],
        ]
    )

    row = ""
    for data in bbb:
        row += str(data) + ","
    print(f"{index+1}/{len(matches)} - {b_index+1}/{len(balls)} done.")

    with lock:
        with open(f"data{argYear}.csv", "a") as file:
            file.write(f"{row}\n")


def getData(matches):
    with open(f"data{argYear}.csv", "a") as file:
        file.write(",".join(header) + "\n")
    threads = []
    for index, match in enumerate(matches):
        balls = getBallsByMatch(match["MatchID"])
        for b_index, b in enumerate(balls):
            thread = threading.Thread(
                target=writeData, args=(match, b, index, b_index, matches, balls)
            )
            thread.start()
            threads.append(thread)
            sleep(0.5)

    for thread in threads:
        thread.join()


ipl2022 = 60
ipl2023 = 107
ipl2024 = 148

matches = []

if argYear == "2022":
    matches = getSchedule(ipl2022)
if argYear == "2023":
    matches = getSchedule(ipl2023)
if argYear == "2024":
    matches = getSchedule(ipl2024)

getData(matches)
