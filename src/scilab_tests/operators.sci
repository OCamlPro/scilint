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
        res(i, j) = int8 (rand() * 256)
      end
    end
  else
    res = int8 (rand() * 256)
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
        res(i, j) = int16 (rand() * 256)
      end
    end
  else
    res = int16 (rand() * 256)
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
        res(i, j) = int32 (rand() * 256)
      end
    end
  else
    res = int32 (rand() * 256)
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
        res(i, j) = rand() * 1000.
      end
    end
  else
    res = rand() * 1000.
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

function name = typenameof (types, v)
  if type (v) == 8 then
    if inttype (v) == 0 then
      name = 'ERR'
      return
    end
  end
  name = 'What ""' + typeof (v) + '""'
  for ty = types
    if ty.tester (v) then
      name = ty.name
    end
  end
endfunction

function res = type_matrix (types, op)
  for i = 1:size(types)
    for j = 1:size(types)
      si = types(i).sampler
      vi = si()
      sj = types(j).sampler
      vj = sj()
      res(i,j) = 'X'
      execstr('res(i,j) = typenameof(types, op.fun(vi, vj))', 'errcatch')
      clear si sj
    end
  end
endfunction

function r = op_plus (x, y), r = x + y, endfunction
function r = op_minus (x, y), r = x - y, endfunction
function r = op_times (x, y), r = x * y, endfunction
function r = op_rdivide (x, y), r = x / y, endfunction
function r = op_ldivide (x, y), r = x \ y, endfunction
function r = op_power (x, y), r = x ^ y, endfunction
function r = op_dot_times (x, y), r = x .* y, endfunction
function r = op_dot_rdivide (x, y), r = x ./ y, endfunction
function r = op_dot_ldivide (x, y), r = x .\ y, endfunction
function r = op_dot_power (x, y), r = x .^ y, endfunction
function r = op_kron_times (x, y), r = x .*. y, endfunction
function r = op_kron_ldivide (x, y), r = x ./. y, endfunction
function r = op_kron_rdivide (x, y), r = x .\. y, endfunction
function r = op_control_times (x, y), r = x *. y, endfunction
function r = op_control_rdivide (x, y), r = x /. y, endfunction
function r = op_control_ldivide (x, y), r = x \. y, endfunction
function r = op_eq (x, y), r = x == y, endfunction
function r = op_ne (x, y), r = x <> y, endfunction
function r = op_or (x, y), r = x | y, endfunction
function r = op_and (x, y), r = x & y, endfunction
function r = op_lt (x, y), r = x < y, endfunction
function r = op_gt (x, y), r = x > y, endfunction
function r = op_le (x, y), r = x <= y, endfunction
function r = op_ge (x, y), r = x >= y, endfunction

op_t = ['op', 'symbol', 'name', 'fun']
ops = list (..
  tlist (op_t, '+', 'Plus', op_plus),..
  tlist (op_t, '-', 'Minus', op_minus),..
  tlist (op_t, '*', 'Times', op_times),..
  tlist (op_t, '/', 'Rdivide', op_rdivide),..
  tlist (op_t, '\', 'Ldivide', op_ldivide),..
  tlist (op_t, '^', 'Power', op_power),..
  tlist (op_t, '.*', 'Dot_times', op_dot_times),..
  tlist (op_t, './', 'Dot_rdivide', op_dot_rdivide),..
  tlist (op_t, '.\', 'Dot_ldivide', op_dot_ldivide),..
  tlist (op_t, '.^', 'Dot_power', op_dot_power),..
  tlist (op_t, '.*.', 'Kron_times', op_kron_times),..
  tlist (op_t, './.', 'Kron_ldivide', op_kron_ldivide),..
  tlist (op_t, '.\.', 'Kron_rdivide', op_kron_rdivide),..
  tlist (op_t, '*.', 'Control_times', op_control_times),..
  tlist (op_t, '/.', 'Control_rdivide', op_control_rdivide),..
  tlist (op_t, '\.', 'Control_ldivide', op_control_ldivide),..
  tlist (op_t, '==', 'Eq', op_eq),..
  tlist (op_t, '<>', 'Ne', op_ne),..
  tlist (op_t, '|', 'Or', op_or),..
  tlist (op_t, '&', 'And', op_and),..
  tlist (op_t, '<', 'Lt', op_lt),..
  tlist (op_t, '>', 'Gt', op_gt),..
  tlist (op_t, '<=', 'Le', op_le),..
  tlist (op_t, '>=', 'Ge', op_ge)..
)

function disp_type_matrix_ocaml (mat, types, op)
  printf ("  %s, [|\n%22s", op.name, '(* ')
  for i = 1:size (types) - 1
    printf ("`%-7s ; ", types(i).name)
  end
  printf ("`%-7s *)\n", types($).name)
  for i = 1:size (mat, 1)
    printf ("    (* `%-7s *) [| ", types(i).name)
    for j = 1:size (mat, 2) - 1
      printf ("`%-7s ; ", mat (i, j))
    end
    printf ("`%-7s |] ;\n", mat (i, $))
  end
  printf ("  |] ;\n")
endfunction

function main ()
  printf ("let binop_type_indexes = [|")
  for i = 1:size (types) - 1
    printf ("`%s ; ", types(i).name)
  end
  printf ("`%s |]\n", types($).name)
  printf ("let binop_type_table = [\n")
  for op = ops
    disp_type_matrix_ocaml (type_matrix (types, op), types, op)
  end
  printf ("]\n")
  quit
endfunction

main ()
