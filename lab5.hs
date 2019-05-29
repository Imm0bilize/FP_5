{-# LANGUAGE FlexibleInstances #-}
import Data.List
 

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = (map (x:) y) ++ y
                where y = subsets xs


amountsI :: [Int] -> [Int]        --суммы без сдачи
amountsI [] = []
amountsI xs = nub(map sum (subsets xs))

check_without::[Int]-> Int -> Bool   --проверка на покупку без сдачи
check_without [] cost = False
check_without (x:xs) cost
        |x == cost = True
        |x /= cost = check_without xs cost

change_pok :: [Int] -> Int ->Int -> Int
change_pok [] _ _ =  0
change_pok (x:xs) cost sum 
        |sum + x < cost = change_pok xs cost (x+sum)
        |sum + x > cost = sum + x

func_blizh_num :: [Int] -> Int ->Int
func_blizh_num [] _ = -1
func_blizh_num (x:xs) num
        |x < num = func_blizh_num xs num
        |x > num = x



index_vhogdeniya_v_arr :: [Int]->Int->Int->Int
index_vhogdeniya_v_arr [] _ _= -1
index_vhogdeniya_v_arr (x:xs) num i 
    |x /= num = index_vhogdeniya_v_arr xs num (i+1)
    |x==num = i
    
        
        
change :: [Int] -> Int -> Int -> Int
change [] _ sum = -sum
change _ 0 sum = sum
change arr sum_for_change sum = 
    if (sum_for_change >= 10) then change (reverse(tail (reverse arr))) (sum_for_change - (head (reverse arr))) (sum + (head (reverse arr)))
    else do
        if (sum_for_change < 10)&&(sum_for_change >=5) then do
            if (index_vhogdeniya_v_arr arr 5 0 > 0) then do
                change ((fst(splitAt (index_vhogdeniya_v_arr arr 5 0) arr)) ++ (tail(snd(splitAt (index_vhogdeniya_v_arr arr 5 0) arr)))) (sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 5 0) arr)))) (sum + (head(snd(splitAt (index_vhogdeniya_v_arr arr 5 0) arr))))
            else do
                if ((index_vhogdeniya_v_arr arr 2 0 > 0) && ((sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) >= 0)) then
                    change ((fst(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)) ++ (tail(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) (sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) (sum + (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr))))
                else
                    change ((fst(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)) ++ (tail(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)))) (sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)))) (sum + (head(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr))))
        else do
            if ((index_vhogdeniya_v_arr arr 2 0 > 0) && ((sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) >= 0)) then
                change ((fst(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)) ++ (tail(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) (sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr)))) (sum + (head(snd(splitAt (index_vhogdeniya_v_arr arr 2 0) arr))))
            else
                change ((fst(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)) ++ (tail(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)))) (sum_for_change - (head(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr)))) (sum + (head(snd(splitAt (index_vhogdeniya_v_arr arr 1 0) arr))))     
        
    
start :: IO()
start = do
    putStrLn "________________________________________________"
    putStrLn ""
    putStrLn "Давай начнем сначала!"
    putStrLn ""
    main


main = do
    putStrLn "Введите номиналы монет продавца в виде списка:"
    pro  <- getLine
    let proarr = sort(read pro :: [Int])                  --список продавца
    putStrLn "Введите номиналы монет покупателя в виде списка:"
    pok  <- getLine
    let pokarr = sort(read pok :: [Int])                  --список покупателя
    putStrLn "Введите стоимость покупки"
    cost <- getLine                                           --цена покупки
    funcmain (proarr) (pokarr) (read cost)

funcmain :: [Int] -> [Int] -> Int -> IO()
funcmain proarr pokarr cost = do
    let sum_without = tail $ sort $ amountsI pokarr           -- список сумм без сдачи
    if (check_without sum_without cost) then do
        putStrLn ""
        putStrLn "Мои поздравления! Покупка сегодня без сдачи!!!"
        start
    else do
        if ((change_pok pokarr cost 0) == 0) then do
            putStrLn "Я, конечно, не эксперт, но, по-моему, тебе не хватает денег("
            start
        else do
            let blizh_num = func_blizh_num (sort (amountsI pokarr)) cost 
                sdacha = change proarr (blizh_num-cost) 0
                ans = "Отдавай за покупку " ++ (show blizh_num)  ++ "р., а сдача будет " ++  (show sdacha) ++ "р." 
            putStrLn ans
            start