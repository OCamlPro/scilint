type format =
  | CODE of string
  | PAR of format list
  | S of string
  | URL of string

let function_warnings_info =
[ PAR [ S
"Local warnings are warnings that can be detected through a local analysis of
one function."
      ]]

let function_warnings = [
001 , "variable not initialized", [0; 1],
[
CODE "function z = f()
  z = cos(x)  // W001: \"x\" not initialized
endfunction"
];

002 , "unused function argument", [0;1],
[
CODE "function z = f(y)// W002: \"y\" is not used
  z = cos(0)
endfunction" ];

003 , "duplicate function argument", [0;2],
[
PAR [ S "Two arguments in a function definition have the same name." ];
PAR [ S "Note: it can only be an error !" ];
PAR [ S "Example:";
CODE "function z = f(a,b,a)// W003: argument \"a\" appears twice
  z = cos(a) + cos(b)
endfunction" ]
];

004 , "duplicate return variable  ", [0;2],
[
PAR [ S "Two return variables in a function definition have the same name."];
PAR [ S "Note: it can only be an error !" ];
PAR [ S "Example:"; CODE
 "function [a,b,a] = f()  // W004: return variable \"a\" appears twice
  a = cos(0)
  b = cos(pi)
  c = cos(pi*2)
endfunction"
]
];

005 , "function argument used as return variable  ", [0;2],
[
PAR [ S "A function argument appears as a return variable."];
PAR [ S "Example:";
      CODE
"function [a] = f(a)  // W005: return variable \"a\" is also an argument
  a = cos(a)
endfunction"]
];

006 , "return variable is never set", [0;2],
[
PAR [ S "A return variable is never set in the function, whatever the path taken.";];
PAR [
S "Example:";
CODE "function [a,b] = f()  // W006: return variable \"a\" is never set
  if( a > 0 ) then
    b = 1;
  else
    b = 0;
  end;
endfunction";
S "Note that, on this example, warnings W001 (variable ``a'' not initialized)
and W007 (return variable used as a variable) should also be displayed.";
]
];

007 , "return variable used as a local variable", [0;2],
[
PAR [ S "A return variable is used in the function as a local variable: return
variables should only be assigned values, never read.";];
PAR [ S "Example:";
CODE "function [a] = f()
  a = 0;
  for i = 1:100,
    a = a +1;   // W007: return variable \"a\" used as a local variable
  end;
endfunction"; ]
];

0 , "variable defined but not used", [],
[];

0 , "use of ``exec'' function     ", [],
[];

0 , "for variable modified in for loop", [],
[];

0 , "for bounds modified during for loop", [],
[];

0 , "keyword used as a variable", [],
[];

0 , "use of return/resume should be avoided", [],
[];

0 , "default value for argument never used", [],
[];

0 , "variable is not initialized in all paths", [],
[];

0 , "field of tlist not defined", [],
[];

]

let file_warnings_info =
[ S "File warnings are warnings that can be detected through a local
analysis of one file. In such an analysis, it is supposed that the
identifiers of functions defined in the file can only be used to call
these functions." ]

let file_warnings = [
0, "duplicate function identifier", [],
[
PAR [ S "The same name is used by two definitions of functions in two different
files." ];

];

0, "too many arguments in function call", [],
[
PAR [ S
"A call to a function defined in the same file provides too many
arguments, compared to the number specified in the function definition." ];

PAR [ S
"Extra arguments will usually not be used. Changing the behavior of a
function depending on its number of arguments is usually a bad
practice."; ];

PAR [ S "Example:";
CODE "function [c] = max(a,b)
  if( a > b ) then
    c = a;
  else
    c = a;
  end;
endfunction;
x = max(1,2,3);  // W 201 : \"max\" used with too many arguments"];
];

0, "too few arguments in function call ", [],
[
PAR [ S "A call to a function defined in the same file does not provide all the
arguments specified in the function definition."; ];
PAR [ S "Changing the behavior of a function depending on its number of
arguments is usually a bad practice." ];
PAR [ S "Example:";
CODE "function [c] = max(a,b)
  if( a > b ) then
    c = a;
  else
    c = a;
  end;
endfunction;
x = max(1);  // W 202 : \"max\" used with too few arguments"];
];

0, "redefinition of primitive function", [], [];
0, "redefinition of standard library function", [], [];
0, "labeled argument does not exist", [], [];
0, "toplevel expression in file", [], [];
]

let global_warnings_info = [
S "Global warnings are warnings that can be detected through a global
analysis of a whole project, i.e. knowning all the files used in the
project."
]

let global_warnings = [
0, "undefined variable or function", [], [];
]

let typing_warnings_info =
[ S "" ]

let typing_warnings = [
0, "variable with different types", [], [];
0, "matrix is growing in loop", [], [];
0, "matrix with different dimensions", [], [];
]

let style_warnings_info =
[ PAR [
    S "Style warnings are warnings caused by not following the style conventions.";];
  PAR [
    S "See ";
    URL "http://wiki.scilab.org/Code%20Conventions%20for%20the%20Scilab%20Programming%20Language";
  ]
]

let style_warnings = [
 601, "functions should start with lowercase", [], [];
 602, "functions should not contain numeric values", [], [];
]


let warnings = [
  "Function Warnings",
  function_warnings_info,
  function_warnings;

  "File Warnings",
  file_warnings_info,
  file_warnings;

  "Global Warnings",
  global_warnings_info,
  global_warnings;

  "Typing Warnings",
  typing_warnings_info,
  typing_warnings;

  "Style Warnings",
  style_warnings_info,
  style_warnings;

]
