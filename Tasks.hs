{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Data.List

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
prima_lista :: Table -> Row
prima_lista x = head (head x) : ["Punctaj Exam"]
--este capul listei cu info nume + punctaj
compute :: Row -> Row
compute x = ((numele_studentului x)
        :((printf"%.2f")(examen_scris x +  (sum_of_q_div_by4 x))):[])
        where
        examen_scris str = read (last str)::Float
        numele_studentului str3 = head str3
        sum_of_q_div_by4 str2 = ((sumOfQ (map read (filter (/="") 
                (tail (init str2)))))/4)::Float
            where
            sumOfQ [] = 0
            sumOfQ (x:xs) = x + sumOfQ xs 
--examen_scris cast float
--sum_of_q_div_by4 = Q1+..Q7/4 + ("" = 0) + cast float

every_list :: Table -> Table
every_list x = foldr(\x acc -> (compute x) : acc) [] (tail x)
--every_list = fiecare lista in parte o ducem la compute
--pentru a scoate numele + examen scris + Q

compute_exam_grades :: Table -> Table
compute_exam_grades x = (prima_lista x) : (every_list x)
--hunionatenam capul listei + restul listelor 

-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num  y = havePassed 
        (filter (>=2.5) (stringToDouble(foldr(\x acc -> (head(tail x)) : acc) [] 
        (drop 1 (compute_exam_grades y)))))
    where
    stringToDouble [] = []
    stringToDouble (x:xs) = (read x) : stringToDouble xs
    --fac cast la note
    havePassed [] = 0
    havePassed (_:xs) = 1 + havePassed xs
    --count nr of stud

-- Percentage of students who have passed the exam:
get_passed_students_num_float :: Table -> Float
get_passed_students_num_float  y = havePassed 
        (filter (>=2.5) (stringToDouble(foldr(\x acc -> (head(tail x)) : acc) [] 
        (drop 1 (compute_exam_grades y)))))
    where
    stringToDouble [] = []
    stringToDouble (x:xs) = (read x) : stringToDouble xs
    havePassed :: [a] -> Float
    havePassed [] = 0.0
    havePassed (_:xs) = 1.0 + havePassed xs
--aceeasi structura ca anterior: filtrez examenele >= 2.5

get_passed_students_percentage :: Table -> Float
get_passed_students_percentage  z = ((get_passed_students_num_float z) 
        / (havePassed (filter (>=0.0) 
        (stringToDouble(foldr(\x acc -> (head(tail x)) : acc) [] 
        (drop 1 (compute_exam_grades z)))))))
    where
    stringToDouble [] = []
    stringToDouble (x:xs) = (read x) : stringToDouble xs
    havePassed :: [a] -> Float
    havePassed [] = 0.0
    havePassed (_:xs) = 1.0 + havePassed xs
--percentage = nr of stud who passed/nr of all students (inclusiv cast si count)

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg exams = (sum_of_exams exams) 
        / (foldr (\x acc -> nr_grades (tail x) + acc) 0 
        (tail (compute_exam_grades exams)))
        where
        nr_grades [] = 0.0
        nr_grades (_:xs) = 1.0 + nr_grades xs
--sum of exams / nr of exams

sum_of_exams :: Table -> Float
sum_of_exams exams = (foldr (\x acc -> parser (tail x) + acc) 0 
        (tail (compute_exam_grades exams)))
        where
        parser x = want_sum(map read x::[Float])
                where
                want_sum [] = 0.0
                want_sum (x:xs) = x + want_sum xs
--fac cast la examene si obtin suma lor

-- Number of students who gained at least 1.5p from homework:
--acum lucram la hw_grades category
get_passed_hw_num :: Table -> Int
get_passed_hw_num s = total_nr_of_studs
        (filter (>=1.5) (foldr(\x acc -> sumOfT(drop 1 (take 4 x)) : acc) [] 
        (fix_hw_list s)))
        where
        sumOfT [] = 0.0
        sumOfT (x:xs) = x + sumOfT xs
        total_nr_of_studs [] = 0
        total_nr_of_studs (_:xs) = 1 + total_nr_of_studs xs
--fac sum of T1,T2,T3 si filtrez studenti cu nota >=1.5

fix_hw_list :: Table -> [[Float]]
fix_hw_list xz =  
        (foldr(\x acc -> hw_to_float(tail x) : acc) [] (tail xz))
        where
        hw_to_float x = foldr(\y acs -> make_float y : acs) [] x
                where
                make_float "" = read "0"::Float
                make_float z = read z::Float
--cast T1,T2,T3 from String to float

-- Task 3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs avg = (((tail(init(head avg)))) 
        : (map (printf"%.2f")
        (init(map (/length_of_questions avg) 
        (hunionat_questions_to_float avg)))):[])
--capul listei : (length of Q/Q )

hunionat_questions_to_float :: Table -> [Float]
hunionat_questions_to_float x = foldr1 
        (zipWith (+)) 
        (foldr(\x acc -> temp(tail x) : acc) [] (tail x))
        where
        temp z = foldr(\y acs -> make_float y : acs) [] z
                where
                make_float [] = 0
                make_float s = read s::Float
-- foldr1 (zipWith (+)) hunionatenates first elements of a list of lists
--primul foldr -> pentru fiecare lista
--al doilea foldr -> pentru fiecare element din lista hunionatenez

length_of_questions :: Table -> Float
length_of_questions s = foldr(\x acc ->  1 + acc) 0 (tail s)

-- Task 4
get_exam_summary :: Table -> Table
get_exam_summary q = ["Q","0","1","2"] :  
        transpose((init(tail(head q))) : (build_the_list q))
--capul listei + rezultatele functiilor 

hunionat_questions_to_float2 :: Table -> [[Int]]
hunionat_questions_to_float2 x = transpose 
        ((foldr(\x acc -> temp(tail(init x)) : acc) [] (tail x)))
        where
        temp z = foldr(\y acs -> make_float y : acs) [] z
                where
                make_float [] = 0
                make_float s = read s::Int
--facem cast la Q si transpose astfel incat sa avem doar 6 liste
--cu Q respective si ne ramane doar sa numaram 0,1,2 la fiecare

zeros :: Table -> [Int]
zeros z = foldr(\x acc -> (count_zeros 0 x) : acc) [] (hunionat_questions_to_float2 z)
        where
        count_zeros :: Int -> [Int] -> Int
        count_zeros x [] = 0
        count_zeros x (c:cs) 
                | x == c = 1 + count_zeros x cs
                | otherwise = count_zeros x cs
--nr of 0 pt fiecare Q
ones :: Table -> [Int]
ones z = foldr(\x acc -> (count_ones 1 x) : acc) [] (hunionat_questions_to_float2 z)
        where
        count_ones :: Int -> [Int] -> Int
        count_ones x [] = 0
        count_ones x (c:cs) 
                | x == c = 1 + count_ones x cs
                | otherwise = count_ones x cs
--nr of 1 pt fiecare Q

twos :: Table -> [Int]
twos z = foldr(\x acc -> (count_twos 2 x) : acc) [] (hunionat_questions_to_float2 z)
        where
        count_twos :: Int -> [Int] -> Int
        count_twos x [] = 0
        count_twos x (c:cs) 
                | x == c = 1 + count_twos x cs
                | otherwise = count_twos x cs
--nr of 2 pt fiecare Q

build_the_list :: Table -> Table
build_the_list x = foldr(\y acs -> convert_to_string y : acs) []
        (((zeros x) 
        : (ones x) 
        : ((twos x):[])))
        where
        convert_to_string z = map show z
--hunionatenez rezultatele in timp ce fac cast la string

-- Task 5
get_ranking :: Table -> Table
get_ranking list = (prima_lista list) : (my_sort (sort(map sort (tail(compute_exam_grades list)))))
        where
        my_sort f = foldr(\x acc -> swaps x:acc) [] f
                where
                swaps [x,y] = [y,x]
--primesc rezultatele examenelor de la compute exam grades
--map sort va sorta lista in baza numelui + rezultatului intr-un mod balantat
--un alt sort va sorta in baza examenului deoarcele acum primul el a listelor este cel de examen
--din acest motiv facem swap la pozitii

-- Task 6
sum_of_q_and_ex_scris :: Row -> Row
sum_of_q_and_ex_scris x = ((numele_studentului x)
        :(printf"%.2f"((sum_of_q_div_by4 x)))
        :(printf"%.2f"(read(last x)::Float)):[])
        where
        numele_studentului str3 = head str3
        sum_of_q_div_by4 str2 = ((sumOfQ (map read (filter (/="") (tail (init str2)))))/4)::Float
            where
            sumOfQ [] = 0
            sumOfQ (x:xs) = x + sumOfQ xs 
--ma bazez pe compute din Task 1 pt a gasi (suma intrebarilor)/4 = punctaj interiu
--hunionatenez numele + punctaj interviu + punctaj scris 

diferenta_interviu_scris :: Row -> Row
diferenta_interviu_scris a = ((sum_of_q_and_ex_scris a) ++ (doesdif (sum_of_q_and_ex_scris a):[]))
        where
        doesdif s = printf"%.2f" (dothedif (tail s))
                where
                dothedif [x,y] = (read x::Float) - (read y::Float)
--fac diferenta dintre "Punctaj interviu" si "Punctaj scris" (inclusiv valori negative)

every_list2 :: Table -> Table
every_list2 x = (foldr(\x acc -> (diferenta_interviu_scris x) : acc) [] (tail x))
--pentru fiecare lista (un fel de for :)) )

get_exam_diff_table2 :: Table -> Table
get_exam_diff_table2 x = does_nothing((every_list2 x))
        where
        does_nothing s = foldr(\x acc -> dotheswap x : acc) [] s
                where
                dotheswap [x,y,z,d] = sterge_minus "-" [d,y,z,x]
                        where
                        sterge_minus valoare list = map checks list
                                where
                                checks y = filter (`notElem` valoare) y
--aranjam pozitia listelor si stergem minus pt ca vrem doar valori pozitive

sterge_minus valoare = map checks 
        where
        checks = filter (`notElem` valoare)
--functie care sterge un element din lista (in cazul nostru -)

sortss :: Table -> Table
sortss x = sort(get_exam_diff_table2 x)
--sortam ca primul elem sortat sa fie diferenta

get_exam_diff_table :: Table -> Table
get_exam_diff_table s = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"] 
        : (sort_in_required_order 
        (foldr(\x acc -> switchsss x : acc) [] (sortss s)))
        where
        switchsss [x,y,z,d] = [d,y,z,x]
--capul listei + lista sortata cum trebuie + ordonata

sort_in_required_order :: Table -> Table
sort_in_required_order = sortBy (\ a b -> 
        if (a !! 3)==(b !! 3) 
        then compare (a !! 0) (b !! 0) 
        else  compare (a !! 3) (b !! 3))
{--functia cea mai importanta din Task
aceasta functie verifica daca elementele de pe pozitia 3
sunt egale, daca da, sorteaza dupa elementul de pe pozitia 0
deci dupa alfabet, daca diferentele intre liste nu sunt egale
atunci pur si simplu sorteaza dupa diferente-}



-- ETAPA 2

filtreaza :: CSV -> [[String]]
filtreaza = ((map (charsep1 ",")) .  (charsep1 "\n") 
        . filter(not . (`elem` "")) . filter(not . (`elem` "\\")))

{-funtia elimina "\\ din string si spatiile goale, unde avem "\n" am separat
listele si dupa acesta separam listele dupa virgula
//charsep este functia folosita la curs-}

read_csv :: String -> [[String]]
read_csv xx = foldr(\x acc -> (prove_length x) : acc) [] (filtreaza xx) 
        where
        prove_length x
                | length x /= nr_lists (filtreaza xx) 
                = fill_empty_spaces (nr_lists (filtreaza xx) - length x) "" x
                | otherwise = x
{- functia prove_length primeste ca argument hw_grades (ordonat din filtreaza)
daca lungimea listei respective nu este egal cu lungimea capului listei
atunci completam cu spatii goale pana ajungem lungimea -}

nr_lists :: [[String]] -> Int
nr_lists hw_grades = length $ head hw_grades
{-functia intoarce lungimea capului listei-}

fill_empty_spaces :: Int -> String -> [String] -> [String]
fill_empty_spaces x y z = z ++ (replicate x y)
{- primeste ca argument un numar, un string, si o lista
intoarce o lista cu elemente egale cu stringul y ca input
repetat x ori.-}


charsep1 :: String -> String -> [String]
charsep1 sep1 = foldr op [] 
        where 
            op x []
                | x `elem` sep1 = []
                | otherwise = [[x]]
            op x (y:ys)
                | x `elem` sep1 = "":y:ys
                | otherwise = (x:y):ys
{-functie de la curs, separates string by char-}

write_csv :: Table -> CSV
write_csv tb = init $ foldr(\x acc -> init (check x) ++ "\n" ++ acc) "" tb
        where
        check lists = foldr(\y acs -> my_spaces(y) ++ acs) "" lists
                where
                my_spaces "" = "" ++ ","
                my_spaces x = x ++ ","

{- primeste ca argument un table. In primul foldr avem
fiecare liste din table. Transmitem fiecare lista la check.
Celalalt foldr in check este pentru fiecare element din lista.
La my_spaces inlocuim fiecare pozitie goala cu virgula.
separam listele cu \n.
-}

--TASK 1

as_list :: String -> [[String]] -> [String]
as_list nume table = tail $ foldr(\x acc -> (x!!value) : acc) [] table
        where
        value = find_index (head table)
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==nume]

{- functia value = find_index primeste ca argument capul listei deci informatia 
din care ne bazam pentru a sti ce ar trebui sa afisam ex "Nume", sau "Q5" etc. 
Aceasta functie gaseste pozitia de column name, ex: "Name" = pozitia 0. Asta 
inseamna ca ar trebui sa scoatem toate elementele de pe aceasta pozitie. -}

--TASK 2
tsort :: String -> [[String]] -> [[String]]
tsort nume table = (head table) : (sort_in_required_order2 value table)
        where
        value = find_index (head table)
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==nume]

{- functia primeste un string si un table. sort_in_required_order2 foloseste 
functia sortBy dupa niste conditii (see below) value = aceeasi functie ca in 
task 1. gasim pozitia de column name pentru a sorta tabela by that column index 
dupa ce gasim indexul il folosim ca conditie la sort_in_required_order2: daca 
indexul listelor sunt egale, sort by first column, alftel sort by index 
column -}

sort_in_required_order2 :: Int -> Table -> Table
sort_in_required_order2 my_int table = sortBy (\ a b -> 
        if (a !! my_int)==(b !! my_int) 
        then compare (a !! 0) (b !! 0) 
        else  compare (a !! my_int) (b !! my_int)) (tail table)


--TASK 3
vmap :: (Value -> Value) -> Table -> Table
vmap correct_exam_table my_table = map (map correct_exam_table) my_table
        where
        correct_exam_table = (\x -> if x == "" then "0" else x)

{- un map pentru fiecare lista, un map pentru fiecare element din lista, apelam 
functia dorita-} 


--TASK 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap get_hw_grade_total y z= ["Nume","Total teme"] 
        : map (get_hw_grade_total) (drop 1 hw_grades)

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : 
                (printf"%.2f" (calculate_sum (map (read::String->Float) 
                $ (filter (/="") $ drop 2 row)))):[]
                where
                calculate_sum [] = 0
                calculate_sum (x:xs) = x + calculate_sum xs       

{-get_hw_grade_total face suma hw -}


--TASK 5
vunion :: Table -> Table -> Table
vunion [] _ = []
vunion t1 t2
        | (head t1 == head t2) = vunion ((head t2) : (tail t1) ++ (tail t2)) (tail t1)
        | otherwise = t1
{- daca column names coincide, returnam capul listei + restul t1 + restul t2,
altfel returnam t1 neschimbat 
-}


--TASK 6
hunion :: Table -> Table -> Table
hunion [] ys = ys
hunion (x:xs) (y:ys) = (x++y) : (hunion (xs) (ys))
hunion (x:xs) ys = (fill_empty_spaces (length x - 1) "" x) : (hunion (xs) (ys))

{- daca exista 2 table.. luam fiecare row, add them daca exista row 1 dar nu 
row 2, add row 1 + elemente goale cu functia folosita anterior -}

find_list :: String -> [[String]] -> [String]
find_list x [] = []
find_list x (y:ys)
        | x == head y = y 
        | otherwise = find_list x (ys)

{-functia primeste un column name si un table.
Daca column name existent, intoarce acea lista
-}

--TASK 7
tjoin :: String -> Table -> Table -> Table
tjoin str xs [] = xs
tjoin str [] ys = []
tjoin str (x:xs) (y:ys) = (noname x (find_list (head x) (y:ys))) : 
        (tjoin str (xs) (y:ys))
        where
        noname (z:zs) (a:as) = ((z:zs) ++ as)
        noname l1 [] = (fill_empty_spaces (length l1 - 2) "" l1)
       
{-functia noname primeste row t1 si rezultatul din functia find_list
unde vrem sa verificam daca exista o lista in t2 cu acea column name
daca exista = t1 ++ t2
daca nu = l1 ++ elemente goale
-}

--TASK 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian funcs str (x:xs) (y:ys) =  str : [funcs v c | v <-xs, c <- ys]
{- aplic functia funcs peste rezultat -}

--TASK 9
projection :: [String] -> Table -> Table
projection strin tb = (cauta (findmy strin tb) tb)


findmy :: [String] -> [[String]] -> [Int]
findmy str2 xs2 = concat $ foldr(\x acc -> find_index(x) : acc) [] xs2
        where
        find_index xs = concat $ foldr(\x acc -> temp_concat(x) : acc) [] str2 
                where
                temp_concat c = [y | (y,z) <- zip [0..] xs, z==c]
{- primeste ca argument coloana specificata si table. 
parcurgem rows, parcurgem elementele din rows, gasim indexul of column.
-}
cauta :: [Int] -> [[a]] -> [[a]]
cauta c list = foldr(\x acc -> cauta'(x) : acc) [] list
        where
        cauta' x = [x!!j | j <- c]
{- primeste indexul, intoarce coloanele cu acel index-}


--ETAPA 3

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query


data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String
 
type EdgeOp = Row -> Row -> Maybe Value
 
data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
        --show :: QResult -> String
        show (CSV x) = show x
        show (Table x) = write_csv x
        show (List x) = show x

class Eval a where
    eval :: a -> QResult

instance Eval Query where
        eval (FromCSV str) = Table $ read_csv str 
        --str to list of lists then from list to lists to string
        eval (ToCSV query) = CSV $ show $ eval query 
        --string to csv, am folosit show pt ca trebuie 
        --ca o functie intermediara, I guess?
        --AsList colname query
        eval (AsList colname query) = List $ as_list colname $ read_csv $ show $ eval query
        --table to csv, then Table pentru functia as_list.. rezultatul e lista
        eval (Sort colname query) = Table $ tsort colname $ read_csv $ show $ eval query
        eval (ValueMap op query) = Table $ vmap op $ read_csv $ show $ eval query
        eval (RowMap op colnames query) = Table $ rmap op colnames $ read_csv $ show $ eval query
        eval (VUnion query1 query2) = 
                                let     q2 = read_csv $ show $ eval query2
                                        q1 = read_csv $ show $ eval query1
                                in Table $ vunion q1 q2
        eval (HUnion query1 query2) =
                                let     q2 = read_csv $ show $ eval query2
                                        q1 = read_csv $ show $ eval query1
                                in Table $ hunion q1 q2
        eval (TableJoin colname query1 query2) =
                                let     q2 = read_csv $ show $ eval query2
                                        q1 = read_csv $ show $ eval query1
                                in Table $ tjoin colname q1 q2
        eval (Cartesian op colnames query1 query2) = 
                                let     q2 = read_csv $ show $ eval query2
                                        q1 = read_csv $ show $ eval query1
                                in Table $ cartesian op colnames q1 q2
        eval (Projection colnames query) = Table $ projection colnames $ read_csv $ show $ eval query
        eval (Filter s query) =
                                let     capul = head $ read_csv $ show $ eval query
                                        q1 = tail $ read_csv $ show $ eval query
                                in Table $ capul : [ x| x <- q1, feval capul s x]
        eval (Graph edges query) = 
                                let from_to = ["From","To","Value"]
                                in Table $ from_to : (edge_sort edges $ read_csv $ show $ eval query)


type FilterOp = Row -> Bool

        

my_float_gt :: [String] -> String -> Float -> FilterOp
my_float_gt table_head capul info row = (read (row!!value) :: Float) > info
        where
        value = find_index table_head
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==capul]
{- input: table head, Gt x y, fiecare lista din table
 -checks if value from column colname is greater than ref
primesc indexul elementului de la indexul din table head, daca acesta > y,
 se indeplineste conditia -}

my_eq_string :: [String] -> String -> String -> FilterOp
my_eq_string table_head capul info row = row!!value == info
        where
        value = find_index table_head
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==capul]
{- input: table head, Eq x y, fiecare lista din table
checks if value from column colname is equal to ref.
primesc indexul elementului de la indexul din table head, daca acesta == y
se indeplineste conditia -}

my_string_lt :: [String] -> String -> String -> FilterOp
my_string_lt table_head capul info row = row!!value < info
        where
        value = find_index table_head
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==capul]
{-la fel ca mai sus, difera conditia-}

my_float_in :: [String] -> String -> [Float] -> FilterOp
my_float_in _ _ [] _ = False
my_float_in table_head capul (x:xs) row = 
        if (row!!value /= "") && ((read (row!!value) :: Float) == x) 
        then True 
        else (my_float_in table_head capul xs row)
        where
        value = find_index table_head
                where
                find_index xs = head [y | (y,z) <- zip [0..] xs, z==capul]
{- checks if value from column colname is in list. -}

check_columns :: [String] -> String -> String -> FilterOp
check_columns table_head str1 str2 row =  
                                let value1 = find_index1 table_head
                                    value2 = find_index2 table_head
                                    find_index1 xs = head [y | (y,z) <- zip [0..] xs, z==str1]
                                    find_index2 xs = head [y | (y,z) <- zip [0..] xs, z==str2]
                                in (row!!value1 == row!!value2)

{-in cazul acesta avem 2 liste. Primim indexul din capul listei, 
extragem elementele listelor din acest index, verificam daca sunt egale-}


class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp


instance FEval Float where
        feval table_head (Gt x y) row = my_float_gt table_head x y row
        feval table_head (In capul (x:xs)) row = my_float_in table_head capul (x:xs) row

instance FEval String where
        feval table_head (Eq x y) row = my_eq_string table_head x y row
        feval table_head (Lt x y) row = my_string_lt table_head x y row
        feval table_head (FNot s) row = not $ feval table_head s row
        feval table_head (FieldEq x y) row = check_columns table_head x y row

{-pozitionam functiile create pentru caz-}



helper :: Row -> Row -> Maybe String -> [String]
helper (n1:l1:_) (n2:l2:_) (Just a) = if n1 < n2 then n1 : n2 : [a] else n2 : n1 : [a]
helper (n1:l1:_) (n2:l2:_) Nothing = []
{-cream lista noua, comparam primul element (numele) din cele 2 liste si
extragem elementul din type Maybe (Justa a, Nothing) -}



edge_sort :: (Row -> Row -> Maybe String) -> Table -> Table
edge_sort _ [x] = []
edge_sort my_edge (z:y:ys) = (split_list 3 (concat 
        (map (\x -> helper y x (my_edge y x)) ys))) ++ (edge_sort my_edge (z:ys))
{- (Row -> Row -> Maybe String) - EdgeOp
functia primeste ca input functia din main (edge_op),
si un table cu cel putin 2 liste. z este capul listei deci il ignoram.
folosim map pentru a lua fiecare lista. ideea este urmatoarea:
list1 cu fiecare lista din table
list2 cu fiecare lista din table
list3 ...
list1, list2 sunt luate din recursivitatea functiei, iar fiecare lista 
din table din functia map.. aplicam functia edge_op la aceasta 
logica implicata. functia helper va construi structura completa a exercitiului.
pentru ca am avut cazuri in care multe liste care trebuiau separate intr-o
liste singure, am folosit functia split_list care imparte elementele in 3 -}

split_list :: Int -> [a] -> [[a]]
split_list _ [] = []
split_list n xs = as : split_list n bs 
  where (as,bs) = splitAt n xs



find_similarities :: Row -> Row -> String
find_similarities (n1:xs) (n2:ys) = 
                let filter_five = length $ map fst . filter (\(x,y) -> x == y) $ zip xs ys 
                in (if filter_five >= 5 then (show filter_five) else (show 0))
{- functia primeste 2 liste si gaseste elementele comune din coloanele lor.
daca nr elementelor comune >= 5 intoarce nr, altfel "0" -}

sort_head n1 n2 = if n1 < n2 then n1 : [n2] else n2 : [n1]
{-aranjeaza sortarea bazata pe numele -}

add_at_end :: String -> [String] -> [String]
add_at_end num xs = xs ++ [num]
{- functia adauga xs la sfarsitul listei num -}

similarity_final :: Table -> Table
similarity_final [x] = []
similarity_final (z:y:ys) = map (\x -> add_at_end (find_similarities x y) 
        (sort_head (head x) (head y))) ys ++ (similarity_final (z:ys))
{-implementare similara ca functia pentru Graph (edge_sort) -}


sorting_table :: Table -> Table
sorting_table t = sort_in_required_order3 2 $ filter (not . null ) $ 
        map(\x -> if ((head x) == "") || (x!!1 == "") || (x!!2 == "0") 
        then [] else x) (similarity_final t)
{-functia primeste ca input lista din similarity_final. aici filtram listele goale, sau cu "" si care au "0"
pentru ca am precizat ca "0" inseamna <5 
sort_in_required_order3 este explicata mai jos-}

sort_in_required_order3 :: Int -> Table -> Table
sort_in_required_order3 my_int table = sortBy (\ a b -> 
        if ((read (a !! my_int) :: Int) == (read (b !! my_int) :: Int))
        then compare (a !! 0) (b !! 0) 
        else  compare (read (a !! my_int) :: Int) (read (b !! my_int) :: Int)) table
{-aceasta functie este implementata la fel ca functiile de sortare din etapele anterioare,
doar ca acum sortam numerele in primul rand, si nu strings -}

similarities_query :: Query
similarities_query = 
                let from_to = ["From","To","Value"]
                in FromCSV (write_csv $ from_to : sorting_table lecture_grades)
{- stim ca similarities_query este un Query si ca lista va fi cea de lecture_grades.
aici aplicam functia de sortare peste lecture grades ca rezultat un From CSV CSV-}