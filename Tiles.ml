open Core.Std

class type tiles = 
object 
  val mutable positions : (int * int) list list 
  method create : (int * int) list list 
end

class xpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if n > 1 && n < height && m < (width-1)
      then positions <- [(0,1);(n,m);(n-1,m+1);(n,m+1);(n,m+2);(n+1,m+1)]::positions
      else ();
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 2 1; positions
end

class ipiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < (width - 3) && n <= height 
      then positions <- [(0,2);(n,m);(n,m+1);(n,m+2);(n,m+3);(n,m+4)]::positions
      else (); 
      if n < (height - 3) && m <= width 
      then positions <- [(0,2);(n,m);(n+1,m);(n+2,m);(n+3,m);(n+4,m)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class zpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,3);(n,m);(n,m+1);(n+1,m+1);(n+2,m+1);(n+2,m+2)]::positions
      else (); 
      if n < (height - 1) && m > 2 && m <= width 
      then positions <- [(0,3);(n,m);(n+1,m-2);(n+1,m-1);(n+1,m);(n+2,m-2)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class vpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,4);(n,m);(n+1,m);(n+2,m);(n+2,m+1);(n+2,m+2)]::positions
      else (); 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,4);(n,m);(n+1,m);(n+2,m);(n,m+1);(n,m+2)]::positions
      else (); 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,4);(n,m);(n,m+1);(n,m+2);(n+1,m+2);(n+2,m+2)]::positions
      else (); 
      if n > 2 && n <= height && m < (width - 1)
      then positions <- [(0,4);(n,m);(n,m+1);(n,m+2);(n-1,m+2);(n-2,m+2)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class tpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,5);(n,m);(n,m+1);(n,m+2);(n+1,m+1);(n+2,m+1)]::positions
      else (); 
      if n > 2 && n <= height && m < (width - 1)
      then positions <- [(0,5);(n,m);(n-1,m+1);(n-2,m+1);(n,m+1);(n,m+2)]::positions
      else (); 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,5);(n,m);(n+1,m);(n+2,m);(n+1,m+1);(n+1,m+2)]::positions
      else (); 
      if n > 1 && n < height && m < (width - 1)
      then positions <- [(0,5);(n,m);(n,m+1);(n,m+2);(n-1,m+2);(n+1,m+2)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class wpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,6);(n,m);(n+1,m);(n+1,m+1);(n+2,m+1);(n+2,m+2)]::positions
      else (); 
      if n > 2 && n <= height && m < (width - 1)
      then positions <- [(0,6);(n,m);(n,m+1);(n-1,m+1);(n-1,m+2);(n-2,m+2)]::positions
      else (); 
      if m < (width - 1) && n < (height - 1) 
      then positions <- [(0,6);(n,m);(n,m+1);(n+1,m+1);(n+1,m+2);(n+2,m+2)]::positions
      else (); 
      if n > 1 && n < height && m < (width - 1)
      then positions <- [(0,6);(n,m);(n,m+1);(n+1,m);(n-1,m+1);(n-1,m+2)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 


class lpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width && n < (height - 2) 
      then positions <- [(0,7);(n,m);(n+1,m);(n+2,m);(n+3,m);(n+3,m+1)]::positions
      else (); 
      if m < (width - 2) && n < height 
      then positions <- [(0,7);(n,m);(n+1,m);(n,m+1);(n,m+2);(n,m+3)]::positions
      else (); 
      if m < width && n < (height - 2) 
      then positions <- [(0,7);(n,m);(n,m+1);(n+1,m+1);(n+2,m+1);(n+3,m+1)]::positions
      else (); 
      if n > 3 && n <= height && m < width
      then positions <- [(0,7);(n,m);(n,m+1);(n-1,m+1);(n-2,m+1);(n-3,m+1)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class ypiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width && n < (height - 2) 
      then positions <- [(0,8);(n,m);(n+1,m);(n+2,m);(n+2,m+1);(n+3,m)]::positions
      else (); 
      if m < (width - 2) && n < height 
      then positions <- [(0,8);(n,m);(n,m+1);(n,m+2);(n,m+3);(n+1,m+1)]::positions
      else (); 
      if m < width && n > 1 && n < (height - 1) 
      then positions <- [(0,8);(n,m);(n,m+1);(n-1,m+1);(n+1,m+1);(n+2,m+1)]::positions
      else (); 
      if n > 2 && n < height && m < width
      then positions <- [(0,8);(n,m);(n,m+1);(n+1,m+1);(n-1,m+1);(n-2,m+1)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class upiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width - 1 && n < height
      then positions <- [(0,9);(n,m);(n+1,m);(n+1,m+1);(n+1,m+2);(n,m+2)]::positions
      else (); 
      if m < width && n < height - 1 
      then positions <- [(0,9);(n,m);(n,m+1);(n+1,m);(n+2,m);(n+2,m+1)]::positions
      else (); 
      if m < width - 1 && n < height
      then positions <- [(0,9);(n,m);(n,m+1);(n,m+2);(n+1,m);(n+1,m+2)]::positions
      else (); 
      if m < width && n < height - 1 
      then positions <- [(0,9);(n,m);(n,m+1);(n+1,m+1);(n+2,m+1);(n+2,m)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class ppiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width - 1 && n < height
      then positions <- [(0,10);(n,m);(n+1,m);(n+1,m+1);(n+1,m+2);(n,m+1)]::positions
      else (); 
      if m < width && n < height - 1 
      then positions <- [(0,10);(n,m);(n,m+1);(n+1,m);(n+2,m);(n+1,m+1)]::positions
      else (); 
      if m < width - 1 && n < height
      then positions <- [(0,10);(n,m);(n,m+1);(n,m+2);(n+1,m+1);(n+1,m+2)]::positions
      else (); 
      if m < width && n < height - 1 
      then positions <- [(0,10);(n+1,m);(n,m+1);(n+1,m+1);(n+2,m+1);(n+2,m)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 

class fpiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width - 1 && n < height - 1
      then positions <- [(0,11);(n+1,m);(n,m+1);(n,m+2);(n+1,m+1);(n+2,m+1)]::positions
      else (); 
      if m < width - 1 && n < height - 1 
      then positions <- [(0,11);(n+1,m);(n,m+1);(n+1,m+1);(n+1,m+2);(n+2,m+2)]::positions
      else (); 
      if m < width - 1 && n < height - 1
      then positions <- [(0,11);(n+2,m);(n+2,m+1);(n+1,m+1);(n,m+1);(n+1,m+2)]::positions
      else (); 
      if m < width - 1 && n < height - 1 
      then positions <- [(0,11);(n,m);(n+1,m);(n+1,m+1);(n+2,m+1);(n+1,m+2)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 


class npiece (width : int) (height: int) : tiles = 
object 
  val mutable positions = []
  method create = 
    let rec count n m = 
      if m < width && n < height - 2
      then positions <- [(0,12);(n,m+1);(n+1,m+1);(n+2,m+1);(n+2,m);(n+3,m)]::positions
      else (); 
      if m < width - 2 && n < height 
      then positions <- [(0,12);(n,m);(n,m+1);(n+1,m+1);(n+1,m+2);(n+1,m+3)]::positions
      else (); 
      if m < width && n < height - 2
      then positions <- [(0,12);(n,m+1);(n+1,m+1);(n+1,m);(n+2,m);(n+3,m)]::positions
      else (); 
      if m < width - 2 && n < height 
      then positions <- [(0,12);(n,m);(n,m+1);(n,m+2);(n+1,m+2);(n+1,m+3)]::positions
      else (); 
      if n = height && m = width then () 
      else if n = height then count 1 (m+1) 
      else count (n+1) m in 
    count 1 1; positions 
end 
