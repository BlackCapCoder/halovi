# Tips
This page contains general tips for using Halovi.

## Loops
The loop is probably the most useful language construct in Halovi, it functions as for loops, while loops, if statements, try blocks and breakpoints.

#### Infinite loop
```
<
  eiThis will be printed an infinite number of times
>
```

#### For loop
```
10<
  eiThis will only be printed 10 times
>
```

#### While loops
```
<
  \img.picture ~# Will break the loop if/when the selector is not found
>
```

#### Breakpoint
```
<> ~# Execution will halt here. If the -H flag has been provided, the browser will still be usuable
```

#### Try block
```
1<
  Do something that might fail here
>
```

#### If statement
```
1<
  \#lorem ~# If the lorem element is found ..
  Y       ~# Copy its text to STDOUT
>
```

## The feature I need is not in halovi

Perhaps it is in bash. You can run bash through Vim!

```
~# Sleep 5 seconds
"0e:!sleep 5<CR>
```
