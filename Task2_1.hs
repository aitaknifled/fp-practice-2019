module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Leaf { key :: Integer, value :: v }
               | Node { key :: Integer, value :: v, leftLeaf :: TreeMap v, rightLeaf :: TreeMap v}
               deriving Show
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree k = False
contains (Leaf key value) k =
  if key == k then True
    else False
contains (Node key value leftLeaf rightLeaf) k =
  if key == k then True
    else if contains leftLeaf k == False then contains rightLeaf k
           else True


-- Значение для заданного ключа для типа String
lookupp :: Integer -> TreeMap String -> String
lookupp k EmptyTree = "Not find"
lookupp k (Leaf key value)
  | key == k = value
  | key /= k = "Not find"
lookupp k (Node key value leftLeaf rightLeaf)
  | key == k = value
  | lookupp k leftLeaf == "Not find" = lookupp k rightLeaf
  | lookupp k leftLeaf /= "Not find" = lookupp k leftLeaf

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Leaf key value)
  | k >= key = Node key value EmptyTree (Leaf k v)
  | k < key = Node key value (Leaf k v) EmptyTree
insert (k, v) (Node key value leftLeaf rightLeaf)
  | k >= key = Node key value leftLeaf (insert (k, v) rightLeaf)
  | k < key =  Node key value (insert (k, v) leftLeaf) rightLeaf

-- Удаление элемента по ключу
--(или удаление целой ветки)
remove :: Integer -> TreeMap v -> TreeMap v
remove k EmptyTree = EmptyTree
remove k (Leaf key value)
  | k == key = EmptyTree
  | k /= key = Leaf key value
remove k (Node key value leftLeaf rightLeaf)
  | k == key = EmptyTree
  | k > key = Node key value leftLeaf (remove k rightLeaf)
  | k < key = Node key value (remove k leftLeaf) rightLeaf

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap String -> (Integer, String)
nearestLE k EmptyTree = (0, "")
nearestLE k (Leaf key value) = (0, "")
nearestLE k (Node key value leftLeaf rightLeaf)
  | k == key = difference k leftLeaf rightLeaf
  | k /= key = 
    if | nearestLE k leftLeaf == (0, "") -> nearestLE k rightLeaf
       | nearestLE k leftLeaf /= (0, "") -> nearestLE k leftLeaf -- other лучше
  where
  difference :: Integer -> TreeMap v -> TreeMap v -> (Integer, v)
  difference k EmptyTree (Leaf key value) = (key, value)
  difference k (Leaf key value) EmptyTree = (key, value)
  difference k (Leaf keyL valueL) (Leaf keyR valueR) = minKeyValue keyL valueL keyR valueR

  difference k EmptyTree (Node key value leftLeaf rightLeaf) = (key, value)
  difference k (Node key value leftLeaf rightLeaf) EmptyTree = (key, value)

  difference k (Leaf keyL valueL) (Node keyR valueR leftLeafR rightLeafR) = minKeyValue keyL valueL keyR valueR
  difference k (Node keyL valueL leftLeafL rightLeafL) (Leaf keyR valueR) = minKeyValue keyL valueL keyR valueR
  difference k (Node keyL valueL leftLeafL rightLeafL) (Node keyR valueR leftLeafR rightLeafR) = minKeyValue keyL valueL keyR valueR
  minKeyValue keyL valueL keyR valueR
    | keyR - k >= k - keyL = (keyL, valueL)
    | keyR - k < k - keyL = (keyR, valueR)


-- Построение дерева из списка пар
-- (чтобы сбалансированное построить, нужно находить из оставшегося списка среднее)
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList list = func list emptyTree
  where
  func :: [(Integer, v)] -> TreeMap v -> TreeMap v
  func [] tree = tree
  func ((k, v) : list) tree = func list (insert (k, v) tree)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf key value) = (key, value) : []
listFromTree (Node key value leftLeaf rightLeaf) = append (append ((key, value) : [])  (listFromTree leftLeaf)) (listFromTree rightLeaf)

-- Конкатенация двух списков (задание из task2.2)
append :: [a] -> [a] -> [a]
append [] list2 = list2
append (head : list1) list2 = head : append list1 list2

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean k t = func k (sort head list [] [])
  where
    head : list = listFromTree t
    func 1 (head : l) = head
    func i (head : l) = func (i-1) l
   
sort :: (Integer, v) -> [(Integer, v)] -> [(Integer, v)] -> [(Integer, v)] -> [(Integer, v)]
sort value [] [] [] = [value]
sort (value, v) [] ((head, vh) : []) []
  | value < head = [(value, v), (head, vh)]
  | value >= head = [(head, vh), (value, v)]
sort (value, v) [] [] ((head, vh) : [])
  | value < head = [(value, v), (head, vh)]
  | value >= head = [(head, vh), (value, v)]
sort (value, v) [] ((headL, vl) : left) [] = sort (headL, vl) (append left [(value, v)]) [] []
sort (value, v) [] [] ((headR, vr) : right) = sort (headR, vr) (append right [(value, v)]) [] []
sort (value, v) [] ((headL, vl) : left) ((headR, vr) : right) = append (sort (headL, vl) (append left [(value, v)]) [] []) (sort (headR, vr) right [] [])
sort (value, v) ((head, vh) : list) left right
  | value < head = sort (value, v) list left ((head, vh) : right)
  | value >= head = sort (value, v) list ((head, vh) : left) right
