#use "q3_4.ml";;

module StrDict = Dict(OrderedString);;

let data = [("ten", 10); ("four", 4); ("nine", 9); ("six", 6); ("eleven", 11); ("two", 2); ("fourteen", 14); ("thirteen", 13); ("zero", 0); ("five", 5); ("eight", 8); ("seven", 7); ("one", 1); ("twelve", 12); ("three", 3)];;
let s = StrDict.load data StrDict.empty;;
StrDict.show s;;
StrDict.lookup "one" s;;
let s = StrDict.remove "ten" s;;
let s = StrDict.remove "one" s;;
StrDict.show s;;
StrDict.lookup "one" s;;
