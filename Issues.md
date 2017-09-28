# Issues

1. Stack and heap indexes are handled inconsistently

Stack index is handled with a scheme variable but heap index is stored in a
dedicated register `RSI`. This is kind of ugly.

2. Share constants b/w C and Scheme code. Move to a .h file
