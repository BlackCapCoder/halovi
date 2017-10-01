# Halovi
Halovi is a vi-like language for web scraping and automation. Upon execution it will read a scroll of alien language, summoning headless browsers and neovims to perform dark magic on webpages. That's pretty spooky, thus halovi (as in Halloween).

# Free scrolls

#### Scroll of scraping
Downloads every single person from the Norwegian phone book matching some string
```
o1881.no⏎⁰ifP<⁰/Y<*Y>]]
```

```
o1881.no     ~# Navigate to http://www.1881.no
⁰i           ~# Write arg0 in the search box and press enter
fP           ~# Click the first link starting with 'P' (Personer)
             ~#
<            ~# Begin loop, break on fail
  ⁰/         ~# Search for arg0 and select the first result
  Y          ~# Copy selection to STDOUT
    <        ~# Begin second loop, break on fail
      *      ~# Select next element of same type, fails if no more elements
      Y      ~# Copy selection to STDOUT
    >        ~# End second loop
  ]]         ~# Go to next page, by clicking "next" or ">>" or simular
>            ~# End first loop
```

A more advanced example:
```
oshirts4mike.com⏎fIR⏎few⏎\img⏎yAs⏎\h1⏎n⏎yeElcw
⏎\option⏎<¹An>⏎e"1pqqgJhr/q2@q
```

```
oshirts4mike.com   ~# Open shirts4mike.com
fIR                ~# Click first link containing "IR"
few                ~# Click first link containing "ew"
\img⏎yAs           ~# Output src attribute of image element
\h1⏎n              ~# Select second h1 element and ..
yeElcw
         ~# edit its text in neovim and output:
                   ~#   go one letter to the left of the end of the next ..
                   ~#   word and replace text from current position until .. 
                   ~#   next word with a linebreak.
\option⏎<¹An>      ~# For each option element, append text to the 1 register
e"1pqqgJhr/q2@q⏎   ~# Output result of neovim edit:
                   ~#   paste content of 1 register, start recording macro q,
                   ~#   join current line with the one below, move one step ..
                   ~#   left and replace character under cursor with /,
                   ~#   stop recording macro, and run the q macro twice.
```

For more info, see [DOC.md](/DOC.md).

# Building

```bash
(cd nm && npm install)
pip install neovim
stack build
stack install # To install Halovi globally
stack exec halovi -- -f somefile -i somearg # To run without installing
```

This is going to take a while..

#### Dependencies

```bash
node
npm
stack
neovim
python3
neovim (python)
```

# Note

Halovi is far from done; I am not even close to implementing all the features that I want from it! See [TODO.md](/TODO.md) and feel free to post feature requests.

Halovi currently does not support multiple windows, because I have to wait for [this](https://github.com/GoogleChrome/puppeteer/pull/554).
