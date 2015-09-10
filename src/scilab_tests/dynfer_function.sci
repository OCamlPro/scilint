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
        res(i, j) = int8 (rand() * 30) - 15
      end
    end
  else
    res = int8 (rand() * 30) - 15
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
        res(i, j) = int16 (rand() * 30) -15
      end
    end
  else
    res = int16 (rand() * 30) - 15
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
        res(i, j) = int32 (rand() * 30) - 15
      end
    end
  else
    res = int32 (rand() * 30) - 15
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
        res(i, j) = rand() * 1000 - 500
      end
    end
  else
    res = rand() * 1000 - 500
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
        res(i, j) = rand() * 1000  - 500 + %i * (rand() * 1000 - 500)
      end
    end
  else
    res = rand() * 1000  - 500 + %i * (rand() * 1000 - 500)
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
  if type (v) == 15 then
    name = 'List'
    return
  end
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

function test_call (types, name, args)
  select size (args)
  case 0
    code = execstr('r = ' + name + '()', 'errcatch')
  case 1
    code = execstr('r = ' + name + '(args(1))', 'errcatch')
  case 2
    code = execstr('r = ' + name + '(args(1), args(2))', 'errcatch')
  case 3
    code = execstr('r = ' + name + '(args(1), args(2), args(3))', 'errcatch')
  case 4
    code = execstr('r = ' + name + '(args(1), args(2), args(3), args(4))', 'errcatch')
  end
  if code ~= 0 then
    return
    r = tlist ([ 'error', 'code', 'message' ], code, lasterror())
  end
  nr = typenameof(types, r)
  printf ("  %s, [", name)
  for i = 1:size(args) - 1
      printf (" %s ;", typenameof(types, args(i)))
  end
  if size(args) > 0 then
      printf (" %s ", typenameof(types, args(size(args))))
  end
  printf ("], %s ;\n", nr)
endfunction

function dynfer (name, maxa, samples)
  test_call (types, name, list())
  indexes = (1:size(types))
  tindexes = indexes'
  for it = 1:maxa
    for i = 1:size(tindexes, 1)
      for s = 0:samples
        args = list()
        for j = 1:size(tindexes, 2)
          sampler = types(tindexes(i, j)).sampler
          select floor (rand () * 4)
          case 0, args(j) = sampler ()
          case 1, args(j) = sampler (3, 3)
          case 2, args(j) = sampler (1, 3)
          case 3, args(j) = sampler (3, 1)
          end
          clear sampler
        end
        test_call (types, name, args)
      end
    end
    tmpindexes = []
    for i = 1:size(indexes, 2) do
       chunk = [ (tindexes(:, 1) * 0 + indexes(i)) tindexes ]
       tmpindexes = [ tmpindexes ; chunk ]
    end
    tindexes = tmpindexes
  end
endfunction

dynfer ('cos', 4, 10)
dynfer ('atan', 4, 10)
dynfer ('sqrt', 4, 10)
quit