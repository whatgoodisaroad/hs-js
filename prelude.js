var Haskell = { };

(function(H) {

  H.enumFromTo = function(f, t) {
    var result = [];
    for (var idx = f; idx <= t; ++idx) {
      result.push(idx);
    }
    return result;
  }

  function _toString(n)         { return n.toString(); }
  
  H["(+)"] = function(a, b) { return a + b; };
  H["(-)"] = function(a, b) { return a - b; };
  H["(*)"] = function(a, b) { return a * b; };
  
  H.tuple = function(fst, snd)        { return { fst:fst, snd:snd }; };
  H.tuple3 = function(fst, mid, snd)  { return { fst:fst, mid:mid, snd:snd }; };

  H.curry = function(fn) {
    var args = Array.prototype.slice.call(arguments, 1);

    return function() {
      return fn.apply(
        null, 
        args.concat(
          Array.prototype.slice.call(
            arguments
          )
        )
      );
    };
  },

  // id               :: a -> a
  // id x             =  x
  H.id = function(a)            { return a; }

  H.cons = function(x, xs)      { return [ x ].concat(xs); };
  H["(:)"] = H.cons;

  H.decons = function(arr)      { return { x:arr[0], xs:arr.slice(1) }; };

  // (.)              :: (b -> c) -> (a -> b) -> a -> c
  // f . g            =  \ x -> f (g x)
  H.compose = function(f, g) { return function(x) { return f(g(x)); }; };

  // flip             :: (a -> b -> c) -> b -> a -> c
  // flip f x y       =  f y x
  H.flip = function (f)         { return function(b, a) { return f(a, b); }; };

  H.not = function(expr)        { return !expr; };

  H.recip = function(n)         { return 1 / n; };

  // even             :: (Integral a) => a -> Bool
  // even n           =  n `rem` 2 == 0
  H.even = function(n)          { return n % 2 == 0; };

  // odd              :: (Integral a) => a -> Bool
  // odd              =  not . even
  H.odd = H.compose(H.not, H.even);

  // gcd              :: (Integral a) => a -> a -> a
  // gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
  // gcd x y          =  gcd' (abs x) (abs y)
  //                     where gcd' x 0  =  x
  //                           gcd' x y  =  gcd' y (x `rem` y)
  H.gcd = function(x, y) {
    function _gcd(x, y) {
      if (y == 0)               { return x; }
      else                      { return _gcd(y, (x % y)); }
    };

    if (x == 0 && y == 0)       { throw "gcd 0 0 is undefined."; }
    else                        { return _gcd(Math.abs(x), Math.abs(y)); }
  },

  // lcm              :: (Integral a) => a -> a -> a
  // lcm _ 0          =  0
  // lcm 0 _          =  0
  // lcm x y          =  abs ((x `quot` (gcd x y)) * y)
  H.lcm = function(x, y) {
    if (x == 0 || y == 0)       { return 0; }
    else                        { return Math.abs((x * y) / H.gcd(x, y)); }
  },

  // (^)              :: (Num a, Integral b) => a -> b -> a
  // x ^ 0            =  1
  // x ^ n | n > 0    =  f x (n-1) x
  //                  where f _ 0 y = y
  //                        f x n y = g x n  where
  //                                  g x n | even n  = g (x*x) (n `quot` 2)
  //                                        | otherwise = f x (n-1) (x*y)
  // _ ^ _            = error "Prelude.^: negative exponent"
  H.exp = function(x, n) {
    function _f(x, n, y) {
      function _g(x, n) {
        if (H.even(n))          { return _g(x * x, n / 2); }
        else                    { return _f(x, n - 1, x * y); }
      }

      if (n == 0)               { return y; }
      else                      { return _g(x, n); }
    }

    if (n == 0)                 { return 1; }
    else                        { return _f(x, n - 1, x); }
  };
  
  H["(^)"] = H.exp;

  // (^^)             :: (Fractional a, Integral b) => a -> b -> a
  // x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))
  H.pow = function(x, n) {
    if (n >= 0)                 { return H.exp(x, n); }
    else                        { return H.recip(H.exp(x, -n)); }
  };
  
  H["(^^)"] = H.pow;

  // fromIntegral     :: (Integral a, Num b) => a -> b
  // fromIntegral     =  fromInteger . toInteger
  H.fromIntegral = H.compose(parseInt, _toString);

  // succ x           =  x+1
  H.succ = function(x)          { return (x * 1) + 1; }

  // until            :: (a -> Bool) -> (a -> a) -> a -> a
  // until p f x 
  //      | p x       =  x
  //      | otherwise =  until p f (f x)
  H.until = function(p, f, x) {
    if (p(x))                   { return x; }
    else                        { H.until(p, f, f(x)); }
  };

  // map :: (a -> b) -> [a] -> [b]
  // map f []     = []
  // map f (x:xs) = f x : map f xs
  H.map = function(f, a) {
    if (!a.length)             { return []; }
    else                        { var d = H.decons(a);
                                  return H.cons(f(d.x), H.map(f, d.xs)); }
  };

  // (++) :: [a] -> [a] -> [a]
  // []     ++ ys = ys
  // (x:xs) ++ ys = x : (xs ++ ys)
  H.concatenate = function(a1, a2) { 
    if (!a1.length)             { return a2; }
    else                        { return H.cons(H.decons(a1).x, H.concatenate(a1, a2)); }
  };
  
  H["(++)"] = H.concatenate;

  // filter :: (a -> Bool) -> [a] -> [a]
  // filter p []                 = []
  // filter p (x:xs) | p x       = x : filter p xs
  //                 | otherwise = filter p xs
  H.filter = function(p, a) {
    if (!a.length)              { return []; }
    var d = H.decons(a);
    if (p(d.x))                 { return H.cons(d.x, H.filter(p, d.xs)); }
    else                        { return H.filter(p, d.xs); }
  };

  // concat :: [[a]] -> [a]
  // concat xss = foldr (++) [] xss
  H.concat = H.curry(H.foldr, H.concatenate, []);

  // concatMap :: (a -> [b]) -> [a] -> [b]
  // concatMap f = concat . map f
  H.concatMap = function(f, a)  { return H.compose(H.concat, function (a) { H.map(f, a); })(a); }

  // head             :: [a] -> a
  // head (x:_)       =  x
  // head []          =  error "Prelude.head: empty list"
  H.head = function(xs) {
    if (!xs.length)             { throw "head: empty list"; }
    else                        { return xs[0]; }
  };

  // tail             :: [a] -> [a]
  // tail (_:xs)      =  xs
  // tail []          =  error "Prelude.tail: empty list"
  H.tail = function(xs) { 
    if (!xs.length)             { throw "tail: empty list"; }
    else                        { return x.slice(1); }
  };

  // last             :: [a] -> a
  // last [x]         =  x
  // last (_:xs)      =  last xs
  // last []          =  error "Prelude.last: empty list"
  H.last = function(xs) {
    if      (xs.length == 0)    { throw "last: empty list"; }
    else if (xs.length == 1)    { return xs[0]; }
    else                        { return H.last(H.decons(xs).xs); }
  };

  // init             :: [a] -> [a]
  // init [x]         =  []
  // init (x:xs)      =  x : init xs
  // init []          =  error "Prelude.init: empty list"
  H.init = function(xs) {
    if      (xs.length == 0)    { throw "init: empty list"; }
    else if (xs.length == 1)    { return []; }
    else                        { var d = H.decons(xs); 
                                  return H.last(H.decons(xs).xs); }
  };

  // null             :: [a] -> Bool
  // null []          =  True
  // null (_:_)       =  False
  H.null_ = function(xs)        { return xs.length == 0; };
  
  // length           :: [a] -> Int
  // length []        =  0
  // length (_:l)     =  1 + length l
  H.length = function(a) {
    if (a.length == 0)          { return 0; }
    else                        { return 1 + H.length(H.decons(a).xs); }
  };

  // foldl            :: (a -> b -> a) -> a -> [b] -> a
  // foldl f z []     =  z
  // foldl f z (x:xs) =  foldl f (f z x) xs
  H.foldl = function(f, z, xs) {
    if (xs.length == 0)         { return z; }
    else                        { var d = H.decons(xs); 
                                  return H.foldl(f, f(z, d.x), d.xs); }
  };

  // foldl1           :: (a -> a -> a) -> [a] -> a
  // foldl1 f (x:xs)  =  foldl f x xs
  // foldl1 _ []      =  error "Prelude.foldl1: empty list"
  H.foldl1 = function(f, a) {
    if (a.length == 0)          { throw "foldl1: empty list"; }
    else                        { var d = H.decons(a); 
                                  return H.foldl(f, d.x, d.dx); }
  };

  // scanl            :: (a -> b -> a) -> a -> [b] -> [a]
  // scanl f q xs     =  q : (case xs of
  //                             []   -> []
  //                             x:xs -> scanl f (f q x) xs)
  H.scanl = function(f, q, a) {
    if (xs.length == 0)         { return [ q ]; }
    else                        { var d = H.decons(a); 
                                  return H.cons(q, H.scanL(f(q, d.x), d.xs)); }
  };

  // scanl1           :: (a -> a -> a) -> [a] -> [a]
  // scanl1 f (x:xs)  =  scanl f x xs
  // scanl1 _ []      =  []
  H.scanl1 = function(f, a) {
    if (a.length == 0)          { return []; }
    else                        { var d = H.decons(a); 
                                  return H.scanl(f, d.x, d.xs); }
  };

  // foldr            :: (a -> b -> b) -> b -> [a] -> b
  // foldr f z []     =  z
  // foldr f z (x:xs) =  f x (foldr f z xs)
  H.foldr = function(f, z, a) {
    if (a.length == 0)          { return z; }
    else                        { var d = H.decons(a); 
                                  return f(d.x, H.foldr(f, z, d.xs)); }
  };

  // foldr1           :: (a -> a -> a) -> [a] -> a
  // foldr1 f [x]     =  x
  // foldr1 f (x:xs)  =  f x (foldr1 f xs)
  // foldr1 _ []      =  error "Prelude.foldr1: empty list"
  H.foldr1 = function(f, a) {
    if (a.length == 0)          { throw "foldr1: empty list"; }
    else if (a.length == 1)     { return a[0]; }
    else                        { var d = H.decons(a); 
                                  return f(x, foldr1(f, xs)); }
  };

  // scanr             :: (a -> b -> b) -> b -> [a] -> [b]
  // scanr f q0 []     =  [q0]
  // scanr f q0 (x:xs) =  f x q : qs
  //                      where qs@(q:_) = scanr f q0 xs
  H.scanr = function(f, q0, a) {
    if (a.length == 0)          { return [ q0 ]; }
    else                        { var da = H.decons(a),
                                      qs = H.head(H.scanr(f, q0, d.xs)),
                                      dq = H.decons(qs);
                                  return H.cons(f(da.x, dq.x), qs); }
  };

  // scanr1          :: (a -> a -> a) -> [a] -> [a]
  // scanr1 f []     =  []
  // scanr1 f [x]    =  [x]
  // scanr1 f (x:xs) =  f x q : qs
  //                    where qs@(q:_) = scanr1 f xs
  H.scanr1 = function(f, a) {
    if (a.length == 0)          { return []; }
    else if (a.length == 1)     { return a; }
    else                        { var da = H.decons(a),
                                      qs = H.head(H.scanr(f, q0, d.xs)),
                                      dq = H.decons(qs);
                                  return H.cons(f(da.x, dq.x), H.scanr1(f, qs)); }
  };

  // take                   :: Int -> [a] -> [a]
  // take n _      | n <= 0 =  []
  // take _ []              =  []
  // take n (x:xs)          =  x : take (n-1) xs
  H.take = function(n, a) {
    if (n <= 0)                 { return []; }
    else if (a.length == 0)     { return []; }
    else                        { var d = H.decons(a); 
                                  return H.cons(d.x, H.take(n - 1, d.xs)); }
  };

  // drop                   :: Int -> [a] -> [a]
  // drop n xs     | n <= 0 =  xs
  // drop _ []              =  []
  // drop n (_:xs)          =  drop (n-1) xs
  H.drop = function(n, a) {
    if (n <= 0)                 { return a; }
    else if (a.length == 0)     { return []; }
    else                        { return H.drop(n - 1, H.decons(xs).xs); }
  };

  // splitAt                  :: Int -> [a] -> ([a],[a])
  // splitAt n xs             =  (take n xs, drop n xs)
  H.splitAt = function(n, xs)   { return H.tuple(H.take(n, xs), H.drop(n, xs)); };

  // takeWhile               :: (a -> Bool) -> [a] -> [a]
  // takeWhile p []          =  []
  // takeWhile p (x:xs) 
  //             | p x       =  x : takeWhile p xs
  //             | otherwise =  []
  H.takeWhile = function(p, a) {
    if (a.length == 0)          { return []; }
    var d = H.decons(a);
    if (p(d.x))                 { return H.cons(d.x, takeWhile(p, d.xs)); }
    else                        { return []; }
  };

  // dropWhile               :: (a -> Bool) -> [a] -> [a]
  // dropWhile p []          =  []
  // dropWhile p xs@(x:xs')
  //             | p x       =  dropWhile p xs'
  //             | otherwise =  xs
  H.dropWhile = function(p, a) {
    if (a.length == 0)          { return []; }
    var d = H.decons(a);
    if (p(d.x))                 { return H.dropWhile(p, d.xs); }
    else                        { return a; }
  };

  // span                    :: (a -> Bool) -> [a] -> ([a],[a])
  // span p []            = ([],[])
  // span p xs@(x:xs') 
  //             | p x       =  (x:ys,zs) 
  //             | otherwise =  ([],xs)
  //                            where (ys,zs) = span p xs'
  H.span = function(p, a) {
    if (a.length == 0)          { return H.tuple([], []); }
    var d = H.decons(a),
        yz = H.span(p, d.xs);
    if (p(d.x))                 { return H.tuple(H.cons(d.x, yz.fst), yz.snd); }
    else                        { return H.tuple([], d.xs); }
  };

  // break                   :: (a -> Bool) -> [a] -> ([a],[a])
  // break p                 =  span (not . p)
  // Note: Break is a reserved keyword, hence the underscore.
  H.break_ = function(p, a)      { return H.span(H.compose(H.not, p), a); };

  // lines            :: String -> [String]
  // lines ""         =  []
  // lines s          =  let (l, s') = break (== '\n') s
  //                       in  l : case s' of
  //                                 []      -> []
  //                                 (_:s'') -> lines s''
  H.lines = function(s) {
    if (s.length == 0)          { return []; }
    else { 
      var br = H.break_(
        function(s) { return s == "\n"; }, 
        s
      );
      
      if (br.snd.length == 0)   { return [ br.fst ]; }
      else                      { return H.cons(br.fst, H.lines(H.decons(br.snd).xs)); }
    }
  };

  // words            :: String -> [String]
  // words s          =  case dropWhile Char.isSpace s of
  //                       "" -> []
  //                       s' -> w : words s''
  //                             where (w, s'') = break Char.isSpace s'
  H.words = function(s) {
    var isSpace = function(s) { return /\s/g.test(s); };
    var sp = H.dropWhile(isSpace, s);
    
    if (sp.length == 0)         { return []; }
    else                        { var br = H.break_(isSpace, sp);
                                  return H.cons(br.fst, H.words(br.snd)); }
  };

  // unlines          :: [String] -> String
  // unlines          =  concatMap (++ "\n")
  H.unlines = H.curry(H.concatMap, function(s) { return s + "\n"; });

  // unwords          :: [String] -> String
  // unwords []       =  ""
  // unwords ws       =  foldr1 (\w s -> w ++ ' ':s) ws
  H.unwords = function(ws) { 
    if (ws.length == 0)         { return ""; }
    else                        { return H.foldrl(function(w, s) { return H.concatenate(w, H.cons(" ", s)); }, ws); }; 
  }

  // reverse          :: [a] -> [a]
  // reverse          =  foldl (flip (:)) []
  H.reverse = H.curry(H.foldl, H.flip(H.cons), []);

  // and, or          :: [Bool] -> Bool
  // and              =  foldr (&&) True
  // or               =  foldr (||) False
  H.and = H.curry(H.foldr, function(a, b) { return a && b; }, true);
  H.or = H.curry(H.foldr, function(a, b) { return a || b; }, false);

  // any, all         :: (a -> Bool) -> [a] -> Bool
  // any p            =  or . map p
  // all p            =  and . map p
  H.any = H.compose(H.or, H.curry(H.map));
  H.all = H.compose(H.and, H.curry(H.map));
  
  // elem, notElem    :: (Eq a) => a -> [a] -> Bool
  // elem x           =  any (== x)
  // notElem x        =  all (/= x)
  H.elem = function(x, a)       { return H.any(function(e) { return e == x; }, a); };
  H.notElem = function(x, a)    { return H.all(function(e) { return e != x; }, a); };
  
  // lookup           :: (Eq a) => a -> [(a,b)] -> Maybe b
  // lookup key []    =  Nothing
  // lookup key ((x,y):xys)
  //     | key == x   =  Just y
  //     | otherwise  =  lookup key xys
  H.lookup = function(key, kvps) {
    if (kvps.length == 0)       { return null; }
    else {
      var d = H.decons(kvps);
      if (key == d.fst)         { return d.snd; }
      else                      { return H.lookup(key, d.xs); }
    }
  };
  
  // sum, product     :: (Num a) => [a] -> a
  // sum              =  foldl (+) 0  
  // product          =  foldl (*) 1
  H.sum = H.curry(H.foldl, H["(+)"], 0);
  H.product = H.curry(H.foldl, H["(*)"], 1);

  // maximum, minimum :: (Ord a) => [a] -> a
  // maximum []       =  error "Prelude.maximum: empty list"
  // maximum xs       =  foldl1 max xs
  H.maximum = function(xs) { 
    if (xs.length == 0)         { throw "maximum: empty list"; }
    else                        { return H.foldl1(Math.max, xs); }
  }
  
  // minimum []       =  error "Prelude.minimum: empty list"
  // minimum xs       =  foldl1 min xs
  H.minimum = function(xs) {
    if (xs.length == 0)         { throw "minimum: empty list"; }
    else                        { return H.foldl1(Math.min, xs); }
  };
  
  // zip              :: [a] -> [b] -> [(a,b)]
  // zip              =  zipWith (,)
  H.zip = H.curry(H.zipWith, H.tuple);
  
  // zip3             :: [a] -> [b] -> [c] -> [(a,b,c)]
  // zip3             =  zipWith3 (,,)
  H.zip3 = H.curry(H.zipWith3, H.tuple3);
  
  // zipWith          :: (a->b->c) -> [a]->[b]->[c]
  // zipWith z (a:as) (b:bs)
  //                  =  z a b : zipWith z as bs
  // zipWith _ _ _    =  []
  H.zipWith = function(z, xs, ys) {
    if (xs.length > 0 && ys.length > 0) 
                                { var xd = H.decons(xs),
                                      yd = H.decons(ys);
                                  return H.cons(z(xd.x, yd.x), H.zipWith(z, xd.xs, yd.xs)); }
    else                        { return []; }
  }

  // zipWith3         :: (a->b->c->d) -> [a]->[b]->[c]->[d]
  // zipWith3 z (a:as) (b:bs) (c:cs)
  //                  =  z a b c : zipWith3 z as bs cs
  // zipWith3 _ _ _ _ =  []
  H.zipWith3 = function(z, xs, ys, zs) {
    if (xs.length > 0 && ys.length > 0 && zs.length > 0) 
                                { var xd = H.decons(xs),
                                      yd = H.decons(ys),
                                      zd = H.decons(zs);
                                  return H.cons(z(xd.x, yd.x, zd.x), zipWith(z, xd.xs, yd.xs, zd.xs)); }
    else                        { return []; }
  }
  
  // unzip            :: [(a,b)] -> ([a],[b])
  // unzip            =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
  
  // unzip3           :: [(a,b,c)] -> ([a],[b],[c])
  // unzip3           =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
  //                           ([],[],[])
  
  
  
  
  
})(Haskell);

