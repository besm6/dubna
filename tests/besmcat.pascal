(*P-, T-, S8, U-, Y+*)
program main(input, output);
var str: array 1..80 of char;
_(
    while not eof(input) do _(
        readln(str);
        writeln(' ', str);
    _);
_).
