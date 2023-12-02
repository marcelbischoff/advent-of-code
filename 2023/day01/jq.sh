#!/bin/sh
jq -nR '[inputs/"" | map(tonumber?) | first*10 + last] | add' input.txt
jq -nR '[inputs'\
'| gsub("one"; "1") '\
'| gsub("two"; "2") '\
'| gsub("three"; "3") '\
'| gsub("four"; "4") '\
'| gsub("five"; "5") '\
'| gsub("six"; "6") '\
'| gsub("seven"; "7") '\
'| gsub("eight"; "8") '\
'| gsub("nine"; "9") '\
'/"" | map(tonumber?) | first*10 + last] | add' input.txt
