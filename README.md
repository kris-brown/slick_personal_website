# Personal website building library

## Summary
This repo generates a static website [here](https://web.stanford.edu/~ksb/) using the [Slick generator](https://github.com/ChrisPenner/slick). Furthermore, it does a lot of processing of notes I take on textbooks, converting them into Anki flashcards using the [AnkiConnect](https://ankiweb.net/shared/info/2055492159) API.

## Usage
The code is built by running `stack build`.

To create/update flashcards for `Sketch`,  `stack exec anki -- Sketch`

To update the website, I run `stack exec build-site` and then `rsync -rv docs/ ksb@rice.stanford.edu:afs-home/WWW`.

## TO DO
- Converting math notes/exercises into webpages (in the style of [Project Crazy Project](https://web.archive.org/web/20140327002205/http://crazyproject.wordpress.com/aadf/#df-1)
- Use caching to speed up the process of updating Anki flashcards.