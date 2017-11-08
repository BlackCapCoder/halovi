# Halovi
Halovi is a vi-like language for web scraping and automation. Upon execution it will read a scroll of alien language, summoning headless browsers and neovims to perform dark magic on webpages. That's pretty spooky, thus halovi (as in Halloween).

# Free scrolls

#### Scroll of stalking
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

#### Scroll of scraping
A more advanced example:
```
oshirts4mike.com⏎fIR⏎/ew⏎<C\img⏎yAs⏎\h1⏎nyeElcw<CR>
\option⏎¹Y<n¹A>e"1pqqgJhr/q2@qo⏎ZQn
```

```
oshirts4mike.com   ~# Open shirts4mike.com
fIR                ~# Click "SHIRTS"
/ew                ~# Search for "ew", for each result ..
< C                ~# Click selection, opening in a new window
  \img⏎yAs         ~# Output the src attribute of image
  \h1⏎n            ~# Select second h1 element and ..
  yeElcw<CR>       ~# edit its text in neovim and output:
                   ~#   go one letter to the left of the end of the next ..
                   ~#   word and replace text from current position until .. 
                   ~#   next word with a linebreak.
  \option          ~# Search for option elements
  ¹Y               ~# Copy text to "1 and ..
  <n¹A>            ~# Append the text of all the other option elements on the page to "1
  e"1pqqgJhr/q2@qo ~# Output the result of the neovim edit:
                   ~#   paste content of 1 register, start recording macro q,
                   ~#   join current line with the one below, move one step ..
                   ~#   left and replace character under cursor with /,
                   ~#   stop recording macro, run the q macro twice, and
                   ~#   open a new line at the end of the buffer
  ZQ               ~# Close window
  n >              ~# Go to the next "ew" result, if any
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
