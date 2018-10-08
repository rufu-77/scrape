#!/bin/zsh

set -e
#set -x

export GHCRTS='-A4m -M500m -s'

cd ~/projects/scrape

newReport() {

    # Browse URLs and store links to ads
    $1 | xargs sh -c 'scrape browse +RTS -p -RTS $@ 2>>.browse.stderr >> .browse.stdout || exit 2'

    # Collect each ad content
    scrape collect 2>>.collect.stderr >>.collect.stdout

    # Find interesting content (TODO: rescan content on new criteria)
    rules=$2
    scrape search "$rules" 2>>.search.stderr >>.search.stdout

    # Report in HTML (let browser mark already visited links)
    report=$3
    echo '<html><head><meta charset="utf-8"></head><body>' > "$report" && sqlite3 scrape.db '.read html.sql' >> "$report" && echo '</body></html>' >> "$report"
}

rentByOwner() { echo "https://www.gumtree.pl/s-mieszkania-i-domy-do-wynajecia/krakow/page-$i/v1c9008l3200208p$1?fr=ownr" }
saleByOwner() { echo "https://www.gumtree.pl/s-mieszkania-i-domy-sprzedam-i-kupie/krakow/v1c9073l3200208p$1?df=ownr" }
carByOwner() { echo "https://www.gumtree.pl/s-samochody-osobowe/krakow/v1c9026l3200208p$1?df=ownr" }

realEstateUrls() {
    # Guess recent listing page URLs (TODO: detect placement time)
    for i in `seq 1 3`; do

        echo $(rentByOwner "$i")
        echo $(saleByOwner "$i")

    done

    # Lots for sale by owner
    echo "https://www.gumtree.pl/s-dzialki/krakow/v1c9194l3200208p1?df=ownr"
}

carUrls() { 
    for i in `seq 1 3`; do

        echo $(carByOwner "$i")

    done
}

newReport "realEstateUrls" "pok.rules" "report.html" 
newReport "carUrls" "car.rules" "cars.html" 
