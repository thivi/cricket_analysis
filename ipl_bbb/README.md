# IPL Hawkeye Ball-by-Ball Data Scraper

Async scraper for IPL Hawkeye ball-by-ball data (2022-2025) with trajectory analytics.

## Setup

```bash
python -m venv venv
.\venv\scripts\activate  # Windows
# source venv/bin/activate  # Linux/Mac
pip install -r requirements.txt
```

## Usage

### 1. Scrape Data
```bash
python ipl_bbb_ascrape.py
```
- Scrapes IPL matches from 2022-2025
- Async processing (hours → minutes)
- Outputs: `datasets/data{year}.csv`
- Logs: `logs/ipl_bbb_scrape_{timestamp}.log`

### 2. Clean & Combine
```bash
python synthesize.py
```
- Sanitizes data types
- Removes incomplete rows (>42 null values)
- Combines all years
- Output: `datasets/ipl_hawkeye_data.csv`

## File Structure
```
ipl_bbb/
├── ipl_bbb_ascrape.py    # Main scraper
├── synthesize.py         # Data cleaning
├── datasets/             # Output CSVs
└── logs/                 # Scraping logs
```

Major modifications:

1. Changed URL to fix 'invalid API key' issue
2. Improved the performance - finishes scraping in 300 seconds instead of hours using asynchronous processing
3. Fixed inaccurate fields like batter/bowler names by using sports mechanics fields


