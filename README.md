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

Usage:
```
halovi -f file.scroll -i blackcap
```

For more info, see [DOC.md](/DOC.md).

# Building

```bash
(cd nm && npm install)
stack build
stack install # To install globally
stack exec halovi -- -f somefile -i somearg # To run without installing
```

This is going to take a while..

#### Dependencies

```bash
node
npm
stack
neovim # Not currently, but soon-ish
```

# Note

Halovi is far from done; I am not even close to implementing all the features that I want from it! See [TODO.md](/TODO.md) and feel free to post feature requests.

Halovi currently does not support multiple windows, because I have to wait for [this](https://github.com/GoogleChrome/puppeteer/pull/554).
