# Strings

If a command takes a string as an argument, all trailing characters until a linebreak will be treated as a string. If any of the characters `⁰¹²³⁴⁵⁶⁷⁸⁹` is present, they will be replaced with the contents of their respective registers.
Additionally, any command that takes a string can be prefixed with a register instead. If this is the case, trailing characters will not be interpreted as a string.

`⏎` and `EOF` are treated as linebreaks.

# Registers

A register is a location in memory where you can store data. A register is written `"` plus a single character that is the name of the register. Any character can be used.
When the program is run, the registers `"0` trough `"9` will be populated with the input arguments. `⁰`, `¹` .. `⁹` is equivalent to `"0`, `"1` .. `"9`.

# Loops

A loop begins with the `<` character, and ends with `>` or `EOF`. Whatever is inside the loop will be executed repeatedly an infinite number of times, or until an error is produced.
If prefixed by a number n, the loop will be executed n times, rather than infinite. If anything inside the loop fails the loop will break immediately, and program execution will proceed normally.

# Numbers

Any command may be prefixed with a number. This will repeat the command n times.

# Commands

| Command    | Description                                                                        |
|------------|------------------------------------------------------------------------------------|
| o[String]  | Navigate to a url. If the argument is not a url, google it using google.com/ncr    |
| [Reg]p     | Navigate to a url. If no register is provided, default to `"0`                     |
| i[String]  | Select the first textbox on the page, type the string, and press enter             |
| /[String]  | Search for a regular expression                                                    |
| \\[String] | Search for a css query                                                             |
| f[String]  | Search for a regular expression, and click the first result                        |
| F[String]  | Search for a query, and click the first result                                     |
| n          | Select the next search result. Fails if this is the last                           |
| N          | Select the previous search result. Fails if this is the first                      |
| *          | Select the next element of the same type. You may use n/N after this. May fail     |
| Y          | Copy the text of the current search result to STDOUT. Fails if nothing is selected |
| ]]         | Click the `next` or `>>` or similar link, to navigate to the next page             |
| [[         | Click the `prev` or `<<` or similar link, to navigate to the previous page         |
