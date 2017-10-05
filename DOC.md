# Flags

You can get a list of program flags by running `halovi -h`. It will look something like this:

```
Halovi - Version 0.0.1
  -f FILE    --file=FILE     File with code
  -c CODE    --code=CODE     Program code
  -i STRING  --input=STRING  Program input
  -d         --debug         Enable debugging
  -H         --headful       Enable headful
  -v         --version       Print version
  -h         --help          Show help
```

# Strings

If a command takes a string as an argument, all trailing characters until a linebreak will be treated as a string. If any of the characters `⁰¹²³⁴⁵⁶⁷⁸⁹` is present in the string, they will be replaced with the contents of their respective registers.

Additionally, any command that takes a string can be prefixed with a register instead. If this is the case, trailing characters will not be interpreted as a string.

`⏎` and `EOF` are treated as linebreaks.

# Registers

A register is a location in memory where you can store data. A register is written `"` plus a single character that is the name of the register. Any character can be used.
When the program is run, the registers `"0` trough `"9` will be populated with the program input arguments.

`⁰`, `¹` .. `⁹` is equivalent to `"0`, `"1` .. `"9`.

### Special Registers

* `""` STDOUT

# Loops

A loop begins with the `<` character, and ends with `>` or `EOF`. Whatever is inside the loop will be executed repeatedly an infinite number of times, or until an error is produced.

If prefixed by a number n, the loop will be executed n times, rather than infinite. If anything inside the loop fails the loop will break immediately, and program execution will proceed normally.

# Numbers

Any command may be prefixed with a number. This will repeat the command n times.

# Comments

Comments begin with `~#` and last until the end of the line. Leading spaces will be treated as part of the comment.

# Commands

| Command [type/default] | Description                                                                     |
|------------------------|---------------------------------------------------------------------------------|
| o[String]              | Navigate to a url. If the argument is not a url, google it using google.com/ncr |
| [Reg/0]p               | Navigate to a url. If the argument is not a url, google it using google.com/ncr |
| [Num/0]i[String]       | Select the nth textbox on the page, type the string, and press enter            |
| /[String]              | Search for a regular expression                                                 |
| \\[String]             | Search for a css query                                                          |
| f[String]              | Search for a regular expression, and click the first result                     |
| F[String]              | Search for a query, and click the first result                                  |
| n                      | Select the next search result. Fails if this is the last                        |
| N                      | Select the previous search result. Fails if this is the first                   |
| *                      | Select the next element of the same type. You may use n/N after this. May fail  |
| [Reg/"]Y               | Copy text of selection to register.                                             |
| [Reg/"]yy              | Copy the url of the current window to register.                                 |
| [Reg/"]yy              | Copy the url of the current window to register.                                 |
| [Reg/"]yA[String]      | Find a attribute of selection matching a regex, and yank to register            |
| ]]                     | Click the `next` or `>>` or similar link, to navigate to the next page          |
| [[                     | Click the `prev` or `<<` or similar link, to navigate to the previous page      |
| gu                     | Go up one level                                                                 |
| gU                     | Go to the root of the webpage                                                   |
| [Num/0]gg              | Go to the top of the webpage or nth search result                               |
| [Num/0]G               | Go to the bottom of the webpage or nth from last search result                  |
