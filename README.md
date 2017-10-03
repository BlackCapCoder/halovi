# Halovi
Halovi is a vi-like language for web scraping and automation. Upon execution, it will read a scroll of alien language to summon headless browsers and headless neovims to perform dark magic on webpages. That's pretty spooky, thus halovi.

# Free scrolls

#### Scroll of scraping
Downloads every single person from the Norwegian phone book matching some string
```
o1881.no⏎⁰ifP<⁰/Y<*Y>]]
```

Usage:
```
halovi -f file.scroll -i blackcap
```

It is equivalent to this:
```
o1881.no      Open http://www.1881.no
"0i           Write arg0 in the search box and press enter
fP            Click the first link starting with 'P' (Personer)

qg            Record macro `g`
*             Select next element of same type as the one currently selected
]]            Go to next page (click the next>> button)
@g            Recursively call `g`
q             Stop recording

qq            Record macro `q`
"0/           Search for the text in arg0
Y             Append selected element to STDOUT
@g            Run the `g` macro
@q            Recursively call `q`
q             Stop recording macro `q`
@q            Run macro `q`
```

# Note

Halovi is far from done; I am not even close to implementing all the features that I want from it!
