function res = make_string (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = int8 (varargin(1))
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = 'string'
      end
    end
  else
    res = 'string'
  end
endfunction

function res = is_string (v)
  res = (type (v) == 10)
endfunction

function res = make_int8 (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = int8 (varargin(1))
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = int8 (rand() * 30) + 1
      end
    end
  else
    res = int8 (rand() * 30) + 1
  end
endfunction

function res = is_int8 (v)
  if type (v) == 8 then
    res = (inttype (v) == 1)
  else
    res = %f
  end
endfunction

function res = make_int16 (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = int16 (varargin(1))
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = int16 (rand() * 30) + 1
      end
    end
  else
    res = int16 (rand() * 30) + 1
  end
endfunction

function res = is_int16 (v)
  if type (v) == 8 then
    res = (inttype (v) == 2)
  else
    res = %f
  end
endfunction

function res = make_int32 (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = int32 (varargin(1))
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = int32 (rand() * 30) + 1
      end
    end
  else
    res = int32 (rand() * 30) + 1
  end
endfunction

function res = is_int32 (v)
  if type (v) == 8 then
    res = (inttype (v) == 4)
  else
    res = %f
  end
endfunction

function res = make_float (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = varargin(1)
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = rand() * 1000. + 2.
      end
    end
  else
    res = rand() * 1000. + 2.
  end
endfunction

function res = is_float (v)
  if type (v) == 1 then
    res = isreal (v)
  else
    res = %f
  end
endfunction

function res = make_complex (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = varargin(1)
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = rand() * 1000. + %i * rand() * 1000.
      end
    end
  else
    res = rand() * 1000. + %i * rand() * 1000.
  end
endfunction

function res = is_complex (v)
  if type (v) == 1 then
    res = ~isreal (v)
  else
    res = %f
  end
endfunction

function res = make_bool (varargin)
  rhs = argn (2)
  // function r = rbool (), if rand () > 0.5 then r = %t, else r = %f, end, endfunction
  function r = rbool (), r = %t, endfunction
  if rhs == 1 then
    res = varargin(1)
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = rbool ()
      end
    end
  else
    res = rbool ()
  end
endfunction

function res = is_bool (v)
  res = (type (v) == 4)
endfunction

function res = make_poly (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = varargin(1)
  elseif rhs == 2 then
    res = []
    for i = 1:varargin(1) do
      for j = 1:varargin(2) do
        res(i, j) = $ // TODO: randomize
      end
    end
  else
    res = $
  end
endfunction

function res = is_poly (v)
  res = (type (v) == 2)
endfunction

function res = make_ratio (varargin)
  rhs = argn (2)
  if rhs == 1 then
    res = varargin(1)
  elseif rhs == 2 then
    num = make_poly (varargin(1), varargin(2))
    den = make_poly (varargin(1), varargin(2))
    res = rlist (num, den)
  else
    num = make_poly ()
    den = make_poly ()
    res = rlist (num, den)
  end
endfunction

function res = is_ratio (v)
  if type (v) == 16 then
    res = (v(1)(1) == 'r')
  else
    res = %f
  end
endfunction

type_t = ['type', 'name', 'sampler', 'tester']
types = list (..
  tlist (type_t, 'Int8', make_int8, is_int8),..
  tlist (type_t, 'Int16', make_int16, is_int16),..
  tlist (type_t, 'Int32', make_int32, is_int32),..
  tlist (type_t, 'Float', make_float, is_float),..
  tlist (type_t, 'Complex', make_complex, is_complex),..
  tlist (type_t, 'Ratio', make_ratio, is_ratio),..
  tlist (type_t, 'Poly', make_poly, is_poly),..
  tlist (type_t, 'Bool', make_bool, is_bool),..
  tlist (type_t, 'String', make_string, is_string)..
)

function r = op_test (op, x, y)
  code = execstr('r = x ' + op + ' y', 'errcatch')
  select code
  case 0
    // do nothing
  case 39, error (39) // Incorrect number of input arguments.
  case 41, error (41) // Incompatible output argument.
  case 42, error (42) // Incompatible input argument.
  case 43, error (43) // Not implemented in scilab...
  case 44, error (44) // Wrong argument %d.
  case 52, error (52) // Wrong type for argument #%d: Real matrix expected.
  case 53, error (53) // Wrong type for input argument #%d: Real or complex matrix expected.
  case 54, error (54) // Wrong type for input argument #%d: Polynomial expected.
  case 55, error (55) // Wrong type for argument #%d: String expected.
  case 56, error (56) // Wrong type for argument #%d: List expected.
  case 58, error (58) // Function has no input argument...
  case 59, error (59) // Function has no output.
  case 77, error (77) // %s: Wrong number of input arguments.
  case 78, error (78) // %s: Wrong number of output arguments.
  case 80, error (80) // Incorrect function (argument n: %d).
  case 81, error (81) // %s: Wrong type for argument #%d: Real or complex matrix expected.
  case 82, error (82) // %s: Wrong type for argument #%d: Real matrix expected.
  case 83, error (83) // %s: Wrong type for argument #%d: Real vector expected.
  case 84, error (84) // %s: Wrong type for argument #%d: Scalar expected.
  case 89, error (89) // Wrong size for argument #%d.
  case 90, error (90) // Wrong type for argument #%d: Transfer matrix expected.
  case 91, error (91) // Wrong type for argument #%d: In state space form expected.
  case 92, error (92) // Wrong type for argument #%d: Rational matrix expected.
  case 93, error (93) // Wrong type for argument #%d: In continuous time expected.
  case 94, error (94) // Wrong type for argument #%d: In discrete time expected.
  case 95, error (95) // Wrong type for argument #%d: SISO expected.
  case 97, error (97) // Wrong type for argument #%d: A system in state space or transfer matrix form expected.
  case 105, error (105) // Undefined function.
  case 118, error (118) // Wrong type for argument #%d: Named variable not an expression expected.
  case 140, error (140) // Variable : %s must be a list
  case 144, error (144) // Opération non définie pour les opérandes données.
  case 201, error (201) // %s: Wrong type for argument #%d: Real or complex matrix expected.
  case 202, error (202) // %s: Wrong type for argument #%d: Real matrix expected.
  case 203, error (203) // %s: Wrong type for argument #%d: Real vector expected.
  case 204, error (204) // %s: Wrong type for argument #%d: Scalar expected.
  case 205, error (205) // %s: Wrong size for argument #%d: (%d,%d) expected.
  case 206, error (206) // %s: Wrong size for argument #%d: %d expected.
  case 207, error (207) // %s: Wrong type for argument #%d: Matrix of strings expected.
  case 208, error (208) // %s: Wrong type for argument #%d: Boolean matrix expected.
  case 209, error (209) // %s: Wrong type for argument #%d: Matrix expected.
  case 210, error (210) // %s: Wrong type for argument #%d: List expected.
  case 211, error (211) // %s: Wrong type for argument #%d: Function or string (external function) expected.
  case 212, error (212) // %s: Wrong type for argument #%d: Polynomial expected.
  case 213, error (213) // %s: Wrong type for argument #%d: Working integer matrix expected.
  case 214, error (214) // Argument #%d of %s: wrong type argument, expecting a vector
  case 215, error (215) // %dth argument type must be boolean.
  case 216, error (216) // Wrong type for argument #%d: Boolean or scalar expected.
  case 217, error (217) // Wrong type for argument #%d: Sparse matrix of scalars expected.
  case 218, error (218) // Wrong type for argument #%d: Handle to sparse lu factors expected.
  case 219, error (219) // Wrong type argument #%d: Sparse or full scalar matrix expected.
  case 231, error (231) // Wrong type for first input argument: Single string expected.
  case 10000, error(10000) // bullshit
  else
    r = tlist ([ 'error', 'code', 'message' ], code, lasterror())
  end
endfunction

op_t = ['op', 'symbol', 'name']
ops = list (..
  tlist (op_t, '+', 'Plus'),..
  tlist (op_t, '-', 'Minus'),..
  tlist (op_t, '*', 'Times'),..
  tlist (op_t, '/', 'Rdivide'),..
  tlist (op_t, '\', 'Ldivide'),..
  tlist (op_t, '^', 'Power'),..
  tlist (op_t, '.*', 'Dot_times'),..
  tlist (op_t, './', 'Dot_rdivide'),..
  tlist (op_t, '.\', 'Dot_ldivide'),..
  tlist (op_t, '.^', 'Dot_power'),..
  tlist (op_t, '.*.', 'Kron_times'),..
  tlist (op_t, './.', 'Kron_ldivide'),..
  tlist (op_t, '.\.', 'Kron_rdivide'),..
  tlist (op_t, '*.', 'Control_times'),..
  tlist (op_t, '/.', 'Control_rdivide'),..
  tlist (op_t, '\.', 'Control_ldivide'),..
  tlist (op_t, '==', 'Eq'),..
  tlist (op_t, '<>', 'Ne'),..
  tlist (op_t, '|', 'Or'),..
  tlist (op_t, '&', 'And'),..
  tlist (op_t, '<', 'Lt'),..
  tlist (op_t, '>', 'Gt'),..
  tlist (op_t, '<=', 'Le'),..
  tlist (op_t, '>=', 'Ge')..
)

function name = typenameof (types, v)
  for ty = types
    if ty.tester (v) then
      if size (v) == [1, 1] then
        name = 'Single ' + ty.name
      else
        name = 'Matrix ' + ty.name
      end
      return
    end
  end
  if type (v) == 16 then
    if v(1)(1) == 'error' then
      name = 'Error (' + string (v.code) + ', ""' + v.message + '"")'
    else
      name = 'Unknown ' + v(1)(1)
    end
    return
  end
  name = 'Unknown'
endfunction

function disp_op_types (fp, types, op, argl, argr)
  rhs = argn(2)
  for i = 1:size(types)
    for j = 1:size(types)
      si = types(i).sampler
      if size (argl, 2) == 2 then
        vi = si(argl (1), argl (2))
      else
        vi = si()
      end
      sj = types(j).sampler
      if size (argr, 2) == 2 then
        vj = sj(argr (1), argr (2))
      else
        vj = sj()
      end
      if execstr ('vr = op_test (op.symbol, vi, vj)', 'errcatch') == 0 then
        ni = typenameof(types, vi)
        nj = typenameof(types, vj)
        nr = typenameof(types, vr)
        mfprintf (fp, "  %s, [ %s ; %s ], %s ;\n", op.name, ni, nj, nr)
      end
      clear si sj
    end
  end
endfunction

function main ()
  fp = mopen ('dynference_results.ml', 'w')
  mfprintf (fp, "let collected_types = [\n")
  for op = ops
    disp_op_types (fp, types, op, [], [])
    disp_op_types (fp, types, op, [ 2, 2 ], [])
    disp_op_types (fp, types, op, [], [ 2, 2 ])
    disp_op_types (fp, types, op, [ 2, 2 ], [2, 2])
  end
  mfprintf (fp, "]\n")
  mclose (fp)
  quit
endfunction

main ()