# Scrape -  a very crude and incomplete web scraper.

# Overview

It's purpose is to fetch classifieds from semi-structured listings (it does not
crawl into arbitrary depths), remove duplicates, match content against
white/blacklist rules while ignoring irrelevant portions (like ads and
other inlined classifieds that would play tricks on matching rules).

# Usage

It is intended to be externally scriptable and run as a cron job(s). For this
purpose, you must select desired mode depending on what you want to do. Most
important modes are:

* *Browse* downloads listing and stores contained hrefs for later
* *Collect* downloads and stores cleaned-up content from recently stored urls 
* *Search* looks for matches in recently stored content agains rules

Full functionality requires creating sqlite database using provided script.
Reporting is done by querying underlying sqlite database. This is to that you
can create arbitrary reports and notifications.

For the most basic usage pipe those two debugging modes together:

* *Fetch* which will extract relevant section after downloading given URL
* *Match* which will search stdin for matches given rules

Note that it's currently hardcoded for specific websites.

In the examples directory you can find a cron job that generates a simple web
page with interesting classifieds. You need to customize it before it will run
on your computer.

