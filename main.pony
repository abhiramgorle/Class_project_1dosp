use "collections"
use "math"
actor Main
  new create(env: Env) =>
    try
      let n = env.args(2)?.usize()?
      let k = env.args(3)?.usize()?
      WorkDistubution(env, n + k, k) 
    else
      env.out.print("the correct way of giving input is 'lucas 10000 4'")
    end
//Actors to create the workers and distrubution of work
actor WorkDistubution
  let _env: Env
  let _size_of_chunk: USize
  let _workers: Array[Answerfinder]
  let _total_number: USize


  new create(env: Env, n: USize, k: USize) =>
    _env = env
    _total_number = n
    if n <= 1000 then
      _size_of_chunk = n
    else 
      _size_of_chunk = Math.ceil_function(Math.square_root(n))
    end

    _workers = Array[Answerfinder]

    let num_ofSummers = ((n - 1) / _size_of_chunk) + 1
    for i in Range(0, num_ofSummers) do
      _workers.push(Answerfinder(env, k, i * _size_of_chunk))
    end

    for i in Range(0, num_ofSummers) do
      let start = (i * _size_of_chunk) + 1
      let endp = if ((i + 1) * _size_of_chunk) > n then n else (i + 1) * _size_of_chunk end
      try
        _workers(i)?.chunk_calc(start, endp)
      end
    end

// Math Class to do some
class Math
  // function to fing the square root of numbers
  fun square_root(n: USize): F64 =>
    if n == 0 then return 0 end
    if n == 1 then return 1 end

    var x: USize = n
    var y: USize = (x + 1) / 2
    while y < x do
      x = y
      y = (x + (n / x)) / 2
    end
    x.f64()


  // ceil function for rounding of the flaoting numbers
  fun ceil_function(x: F64): USize =>
    let i = x.trunc().i64()
    if (x > 0) and (x > i.f64()) then
      return (i + 1).usize()
    else
      return i.usize()
    end


actor Answerfinder
  let _env: Env
   let _work_queue: Array[U64]
  let _k: USize
  let _offset: USize
  var _sumans: U64
  var _index: USize
  let _checker: CHeckSquareorNot

  new create(env: Env, k: USize, offset: USize) =>
    _env = env
    _k = k
    _offset = offset
    _work_queue = Array[U64](_k)
    _sumans = 0
    _index = 0
    _checker = CHeckSquareorNot(env)

  be chunk_calc(start: USize, endp: USize) =>
    for i in Range(start, endp + 1) do
      let square = (i.u64() * i.u64())
      add_square(square, start)
    end

  fun ref add_square(square: U64, start: USize) =>
    if _work_queue.size() == _k then
      try
        _sumans = _sumans - _work_queue.shift()?
      end
    end

    _work_queue.push(square)
    _sumans = _sumans + square

    if _work_queue.size() == _k then
      _checker.check_perfect_square(_sumans, _offset + _index)
      _index = _index + 1
    end



actor CHeckSquareorNot
  let _env: Env
  let _results: Array[Bool]

  new create(env: Env) =>
    _env = env
    _results = Array[Bool]

  be check_perfect_square(n: U64, index: USize) =>
    let result = is_perfect_square(n)

    if result == true then
      _env.out.print((index + 1).string())
    end

    _results.push(result)

  fun is_perfect_square(n: U64): Bool =>
    if n == 0 then return true end
    if n == 1 then return true end

    var left: U64 = 1
    var right: U64 = n

    while left <= right do
      let mid = left + ((right - left) / 2)
      let mid_squared = mid * mid

      if mid_squared == n then
        return true
      elseif mid_squared < n then
        left = mid + 1
      else
        right = mid - 1
      end
    end

    false

