# purescript-heap

Heap data structure. Implemented using Leftist heap.

## Usage

```purescript
main = do
  let heap :: Heap Min Int
      heap = empty
             # insert 4
             # insert 5
             # insert 2
             # insert 9
  log $ show $ min heap              -- => 2
  log $ show $ min (deleteMin heap)  -- => 4
```
