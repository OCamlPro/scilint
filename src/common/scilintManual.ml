type format =
  | CODE of string
  | SCODE of string
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
endfunction";

    PAR [ S "Remarks : referencing global variables should be avoided, variables should be passed as arguments. This variable can also have been introduced by a 'resume' in a function called from the current context, or an 'execstr' function"
        ];
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
endfunction"];
    PAR [ S "However, this is often used as an optimization to avoid
a useless copy when returning a matrix." ]
  ];

  006 , "return variable is never set", [0;2],
  [
    PAR [ S "A return variable is never set in the function,
whatever the path taken.";];
    PAR [
      S "Example:";
      CODE "function [a,b] = f()  // W006: return variable \"a\" is never set
  if( a > 0 ) then
    b = 1;
  else
    b = 0;
  end;
endfunction";
      S "Note that, on this example, warnings W001
(variable ``a'' not initialized)
and W007 (return variable used as a variable) should also be displayed.";
    ]
  ];

  007 , "return variable used as a local variable", [0;2],
  [
    PAR [ S "A return variable is used in the function as a local variable:
 return variables should only be assigned values, never read.";];
    PAR [ S "Example:";
          CODE "function [a] = f()
  a = 0;
  for i = 1:100,
    a = a +1;   // W007: return variable \"a\" used as a local variable
  end;
endfunction"; ];
    PAR [ S "However, this is often used as an optimization to avoid
a useless copy when returning a matrix." ]

  ];

  008 , "for variable modified in for loop", [0;3],
  [
    PAR [ S
            "Modifying the variable of a 'for' loop does not change the
loop behavior."; ];
    CODE "for i=1:100
   i=i+2; // W: modifying variable of 'for' loop does not change loop behavior
   disp(i);
end";
  ];

  009, "redefinition of primitive function", [0;3],
  [ PAR [ S "Redefining a primitive is usually a bad idea" ];
    CODE "function disp(x) // W009: redefinition of primitive 'disp'
  disp(x+1)
endfunction"
        ];

  010, "redefinition of local function", [0;3], [
    PAR [ S "The code is redefining a function that was already defined in this scope." ];
    CODE "function f()
  function pr(x), disp(x),endfunction
  function pr(x,y) // W010: redefinition of local function 'pr'
    disp(x,y)
  endfunction
endfunction
"

  ];

  011, "redefinition of library function", [0;3], [
    PAR [ S "The code is redefining a function that was already defined in the toplevel scope." ];
    CODE "function pr(x), disp(x),endfunction
function f()
   function pr(x,y) // W010: redefinition of library function 'pr'
     disp(x,y)
   endfunction
endfunction
"

  ];

  012 , "unexpected string argument", [0;3],
  [
    PAR [ S "Some functions expect a limited list of strings as flags. If another string is passed, it is usually an error." ];
    CODE "function f()
   execstr(\"x=1\", \"x=2\"); // W012: \"x=2\" instead of \"catcherr\"
endfunction";
  ];

  013 , "primitive with too many arguments", [0;3],
  [
    PAR [ S "Some primitives expect a limited number of arguments. Providing more arguments will usually trigger an error at runtime." ];
    CODE "function f(a,b)
   disp(strcat('a=',a,'b=',b); // W013: 'strcat' expects fewer arguments
endfunction";
  ];

  0 , "variable defined but not used", [],
  [
    CODE "function f()
  x = 1;  // W : variable \"x\" defined but not used
  y = 2;
  disp(y);
endfunction";
      PAR [ S "The variable might be used in another function, but it is
a bad practice to pass values like that instead of by argument." ];
  ];

  0 , "variable redefined before being used", [],
  [
    CODE "function f()
  x = 1;  // W : variable \"x\" redefined before being used
  x = 2;
  disp(x);
endfunction"

  ];

  0 , "use of ``execstr'' function     ", [],
  [
    CODE "x = execstr(instr, 'catcherr'); // W: avoid use of 'execstr' "
  ];

  0 , "for bounds modified during for loop", [],
  [
    PAR [ S "Modifying the bounds of a loop does not change the behavior of
the 'for' loop"; ];
    CODE "a = 3;
for i=1:a
  disp(i);
  a = 5; // W: modifying bound of 'for' loop does not change loop behavior
end"
  ];

  0 , "keyword used as a variable", [],
  [
    PAR [ S "Keywords should not be used as names of variables." ];
    CODE "function f(if) // W : 'if' should not be used as a variable
  disp(if)
endfunction";
  ];

  0 , "use of return/resume to define variables should be avoided", [],
  [
    CODE "function x_is_one()
  x = resume(1) // W : 'resume' should not be used
endfunction";
  ];

  0 , "variable is not initialized in all paths", [],
  [
    CODE "function f(a,b)
  if (a > 3) then
    x = b
  end
  disp(x) // W: 'x' is not initialized in all paths
endfunction"
  ];

  0 , "field of tlist not defined", [],
  [
    CODE "function f()
  x = tlist([\"obj\", \"a\",\"b\"])
  x.c = 3 // W : \"c\" was not declared inside the tlist declaration
endfunction"
  ];

  0 , "break outside of loop", [],
  [
    CODE "function f()
  x = 1
  break; // W : break outside of loop
  disp(x);
endfunction"
  ];

  0 , "continue outside of loop", [],
  [
    CODE "function f()
  x = 1
  continue; // W : continue outside of loop
  disp(x);
endfunction"
  ];

  0, "labeled argument provided twice", [],
  [
    CODE "function f(a, b, c) endfunction
f(a, b, b=3) // W : argument \"b\" provided twice"
  ];

  0, "unlabeled argument after labeled argument", [],
  [
    CODE "f(a, b=2, c) // W : unlabeled argument after labeled argument \"b\""
  ];

  0, "missing parenthesis for function call", [],
  [
    CODE "[a,b] = f // W : you should use \"f()\" instead of \"f\""
  ];

  0, "non void return value of function not used", [],
  [
    CODE "function [x]=f() x=1; endfunction
f(); // W : non void return value of function not used
"
  ];


]

let file_warnings_info =
[ S "File warnings are warnings that can be detected through a local
analysis of one file. In such an analysis, it is supposed that the
identifiers of functions defined in the file can only be used to call
these functions." ]

let file_warnings = [

  0, "duplicate function identifier", [],
  [
    PAR [ S "The same name is used by two definitions of functions in
two different files." ];
  ];

  0 , "type not defined in redefinition", [],
  [
    CODE "
  x = tlist([\"object\", \"a\",\"b\"])
function %objcet_p(mytlist) // 'objcet was not defined as a type
  mprintf(\"a=%s\\n\", x.a);
endfunction"
  ];

  0, "too many arguments in function call", [],
  [
    PAR [ S
            "A call to a function defined in the same file provides too many
arguments, compared to the number specified in the function definition." ];

    PAR [ S
            "Extra arguments will usually not be used. Changing the behavior
of a function depending on its number of arguments is usually a bad
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

  0, "labeled argument does not exist", [], [
    CODE "function f(a, b)
endfunction
f(1, c=3)"
  ];

  0, "toplevel expression in file", [], [
    PAR [ S "It is better to avoid execution of expressions in files, and
to call an initialization function at the beginning."
        ]];

  0, "labeled argument in varargin", [], [
    PAR [ S "Labeled arguments are not allowed in varargin" ];
    CODE "function f(a, b, varargin)
endfunction
f(1,2, c=3)"
  ];

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
 601, "Function names should start with lowercase", [0;4], [] ;
 602, "Function names should not contain digits", [0;4], [] ;
 603, "Inconsistent string delimiters", [0;4], [
   PAR [
     S "A string starting with a " ;
     SCODE "\"" ;
     S " (resp. a " ;
     SCODE "'" ;
     S ") should end with a " ;
     SCODE "\"" ;
     S " (resp. a " ;
     SCODE "\'" ;
     S "). \
        This warning is also output when a delimiter is escaped with two different characters \
        such as in " ;
     SCODE "\"this is a quote: \"'\"" ;
     S " which should be written " ;
     SCODE "\"this is a quote: ''\""
   ]
 ] ;
 604, "Inconsistent matrix delimiters", [0;4], [
   PAR [
     S "A string starting with a " ;
     SCODE "[" ;
     S " (resp. a " ;
     SCODE "{" ;
     S ") should end with a " ;
     SCODE "]" ;
     S " (resp. a " ;
     SCODE "}" ;
     S ")."
   ]
 ] ;
 605, "Inconsistent matrix separators", [], [
   PAR [
     S "This warning is issued when spaces and comas are both used to \
        separate the cells of a single matrix litteral which can lead to misreadings." ;
     S "To make it disappear, simply add comas between all pairs of consecutive cells."
   ] ;
   PAR [
     S "This warning is also issued when semicolons and line breaks are both used to \
        separate the rows of a single matrix litteral." ;
     S "To make it disappear, simply add semicolons at the end of every row or replace \
        all semicolons by line breaks."
   ]
 ] ;
 606, "Spaces in operator", [0 ;4], [
   PAR [ S "Scilab's interpreter accepts spaces inside some operators, such as " ;
         SCODE ". * ." ; S " which is not a good idea." ]
 ] ;
 607, "Spaces around dot", [0 ;4], [
   PAR [ S "In some cases, a space before the dot of a field access gives a different \
           sense to the code line. For instance, " ;
         SCODE "v.f = 3" ;
         S " assigns 3 to the field f of the variable v, while " ;
         SCODE "v . f = 3" ;
         S " tests is the value of this field is 3." ;
         S "It is thus a good idea to avoid spaces around all dots."
   ]
 ] ;
 608, "Missing function parameters", [0 ;4], [
   PAR [ S "It is recommended to make explicit an empty list of arguments by writing " ;
         SCODE "()" ;
         S " instead of just breaking the line after the function name."
   ]
 ] ;
 609, "Missing catch", [0 ;4], [
   PAR [ S "A try block without a catch statement is probably useless." ]
 ] ;
 610, "Misused keyword", [0 ;4], [
   PAR [ S "Using a language keyword as a variable, field name or anything but a keyword \
            makes code difficult to read." ] ;
   PAR [ S "To correct this, rename the variable." ]
 ] ;
 611, "Keyword as shell arg", [0 ;4], [
   PAR [ S "Using a language keyword unquoted inside a shell call such as in " ;
         SCODE "disp end";
         S " makes code difficult to read." ] ;
   PAR [ S "To correct this, surround the keyword with quotes, such as " ;
         SCODE "disp 'end'" ]
 ] ;
 612, "Deprecated", [0 ;4], [
   PAR [ S "This warning is issued when a deprecated function, operator or \
            construct is encountered." ]
 ] ;
 613, "Ambiguous dot at left of operator", [0 ;4], [
   PAR [ S "This warning is issued when a sequence similar to " ;
         SCODE "3./2" ;
         S " is encountered, which is interpreted as " ;
         SCODE "3. / 2" ;
         S " but could be read as " ;
         SCODE "3 ./ 2" ] ;
   PAR [ S "To correct it, just insert a space if you meant a dotted operator \
            or remove the dot otherwise." ]
 ] ;
 614, "Ambiguous dot at right of operator", [0 ;4], [
   PAR [ S "This warning is issued when a sequence similar to " ;
         SCODE "3/.2" ;
         S " is encountered, which is interpreted as " ;
         SCODE "3 / .2" ;
         S " but could be read as " ;
         SCODE "3 /. 2" ] ;
   PAR [ S "To correct it, just insert a space in the right place or surround \
            the right hand side with parentheses." ]
 ] ;
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

let version = "0.3"

let changelog = [
    "0.1", "Sep 20, 2013", [
      "General", [
        "First public appearance of Scilint" ;
      ];
      "Warnings", [
        "Warning W001: variable not initialized";
        "Warning W002: unused function argument";
      ];
    ];

    "0.2", "Nov 12, 2013", [
      "Warnings:", [
        "Warning W003: duplicate function argument";
        "Warning W004:duplicate return variable";
        "Warning W005: function argument used as return variable";
        "Warning W006: return variable is never set";
        "Warning W007: return variable used as a local variable";
      ]
    ];

    "0.3", "Nov 22, 2013", [
      "General:", [
        "Support for FireHose output format";
      ];

      "Arguments:", [
        "New argument -I, to add a directory to the search path";
        "New argument -xml, to output in FireHost format";
      ];

      "Fixes:", [
        "Remove assertion failures during analysis";
      ];

      "Warnings:", [
        "Warning W008: for variable modified in for loop";
        "Warning W009: redefinition of primitive function";
        "Warning W010: redefinition of local function";
        "Warning W011: redefinition of library function";
        "Warning W012: unexpected string argument";
        "Warning W013: primitive with too many arguments";
      ];
    ];

    (* To insert for next release:
       "Warning W601: function names should start with lowercase";
       "Warning W602: function names should not contain digits";
       "Warning W603: Inconsistent string delimiters";
       "Warning W604: Inconsistent matrix delimiters";
       "Warning W606: Spaces in operator";
       "Warning W607: Spaces around dot";
       "Warning W608: Missing function parameters";
       "Warning W609: Missing catch";
       "Warning W610: Misused keyword";
       "Warning W611: Keyword as shell arg";
       "Warning W612: Deprecated";
       "Warning W613: Ambiguous dot at left of operator";
       "Warning W614: Ambiguous dot at right of operator"; *)
  ]


