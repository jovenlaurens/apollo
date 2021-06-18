module Text exposing (changeIndexToNewOne, showText, textBase)

import Maybe exposing (withDefault)

--大致思路：用一个函数完成文本之间的显示转换，输入当前界面和转换值，输出相应的部分

type alias Text =
    { index : Int
    , content : String 
    }

showText : Int -> String
showText ind =
    let
        target = (List.filter (\x -> x.index == ind) textBase) 
                    |> List.head 
                    |> withDefault (Text -99 "a")
        
    in
        target.content

trigue4 : Int -> Int -> Int -> Int -> Int -> Int
trigue4 trigue a b c d=
    case trigue of
        1 -> a
        2 -> b
        4 -> d
        _ -> c

trigue5 : Int -> Int ->Int -> Int -> Int -> Int -> Int
trigue5 trigue a b c d e=
    case trigue of
        1 -> a
        2 -> b
        3 -> c
        4 -> e
        _ -> d


changeIndexToNewOne : Int -> Int -> Int
changeIndexToNewOne old trigue =
--trigue 的话，摁button时候就是0，输了时候调用就是2，赢了是1
    if trigue == -99 then
        old
    else
    case old of
        0 -> 
            trigue4 trigue 11 6 1 8
        1 -> trigue4 trigue 11 6 2 8
        2 -> trigue4 trigue 11 6 3 8
        3 -> trigue4 trigue 11 6 4 8
        4 -> trigue4 trigue 11 6 5 8
        5 ->
            trigue4 trigue 11 6 7 8
        6 -> 
            trigue4 trigue 11 6 7 8
        7 -> trigue4 trigue 11 6 7 8
        8 -> trigue4 trigue 11 6 7 8
        11 -> trigue5 trigue 21 17 19 12 20
        12 -> trigue5 trigue 21 17 19 13 20
        13 -> trigue5 trigue 21 17 19 14 20
        14 -> trigue5 trigue 21 17 19 15 20
        15 -> trigue5 trigue 21 17 19 16 20
        16 -> trigue5 trigue 21 17 19 18 20
        17 -> trigue5 trigue 21 17 19 18 20
        18 -> trigue5 trigue 21 17 19 18 20
        19 -> trigue5 trigue 21 17 19 18 20
        20 -> trigue5 trigue 21 17 19 18 20
        21 -> trigue5 trigue 31 24 26 22 28
        22 -> trigue5 trigue 31 24 26 23 28
        23 -> trigue5 trigue 31 24 26 25 28
        24 -> trigue5 trigue 31 24 26 25 28
        25 -> trigue5 trigue 31 24 26 25 28
        26 -> trigue5 trigue 31 24 26 25 28
        28 -> trigue5 trigue 31 24 26 25 28
        27 -> trigue5 trigue 40 36 27 41 28
        31 -> trigue5 trigue 40 36 27 32 42
        32 -> trigue5 trigue 40 36 27 33 42
        33 -> trigue5 trigue 40 36 27 34 42
        34 -> trigue5 trigue 40 36 27 35 42
        35 -> trigue5 trigue 40 36 27 41 42
        41 -> trigue5 trigue 40 36 27 41 42
        42 -> trigue5 trigue 40 36 27 41 42
        36 -> 37
        37 -> 38
        38 -> 39
        39 -> 40
        40 -> 40
        _ -> 0




        
            



textBase : List Text
textBase = 
    [ Text 0 "Apollo, by Ocean Cat Studio-Default"
    , Text 1 "In 2077, people have occupied most area of the solar system. A huge interplanetary empire has been established. You, one of the freshmen in the most powerful space fleet of the empire, when something strange happened."
    , Text 2 "A kind of secret particle ejected by the sun, started to attack the world with its high speed and high temperature. Your team is assigned to drive the firmest spacecraft, Apollo, to protect everything in the world."
    , Text 3 "By pressing the <- or -> on the keyboard, Apollo will turn anticlockwise or clockwise around the track. It can make the particle bounce back into the inner area, bounce back, again and again. "
    , Text 4 "Every time the particle collides with the sun, its intensity will decrease and become darker. The proton keeps moving until the intensity turns into zero, when your task is partly finished."
    , Text 5 "Any possibility exists, so try your best to protect the solar system, our permanent family! (Press New Game to Start!)"
    , Text 6 "This time you failed, and you will come back to the beginning of this war. (Press Resume to restart this level)"
    , Text 7 "Apollo, by Ocean Cat Studio\n--Level 1"
    , Text 8 "Oh no, the spacecraft escaped successfully! You lose one chance! (Press Resume to continue)"
    , Text 11 "After a “fierce” war, you eventually returned to your home and got a piece of short but precious leisure time. "
    , Text 12 "Unfortunately, terrible news came that Apollo was damaged in an unexpected destructive solar activity!"
    , Text 13 "However, we won’t lose hope, since a bigger and stronger track of spacecrafts have been built far away the sun, so the track of the earth is included."
    , Text 14 "Right, the more powerful Apollo, comes back!"
    , Text 15 "You have another spacecraft, controlled by A and D for turning around anticlockwise or clockwise. The track is the same, offering you more freedom to control it."
    , Text 16 "But the proton will be also stronger! Also, do not touch the Earth! You know the sun is getting worse, so please try harder."
    , Text 17 "This time you failed, and you will come back to the beginning of this war. (Press Resume to restart this level)"
    , Text 18 "Apollo, by Ocean Cat Studio\n--Level 2"
    , Text 19 "Bad news! The proton hit the earth, and everything will remake. (Press Resume to restart this level)"
    , Text 20 "Oh no, the spacecraft escaped successfully! You lose one chance! (Press Resume to continue)"
    , Text 21 "Congratulations, my son! Perhaps you may figure out your identity. "
    , Text 22 "All right, my dear Apollo. You’re born for the world, for the empire. As the descendant of the royalty, you should be responsible for the safety of the entirety, so keep fighting, fighting for the royal glory!"
    , Text 23 "Another bad news is that, the latest Apollo seems to have some fault: the function of geostationary orbit broke out, and the Earth start to rotate. So, good luck, my child!"
    , Text 24 "This time you failed, and you will come back to the beginning of this war. (Press Resume to restart this level)"
    , Text 25 "Apollo, by Ocean Cat Studio\n--Level 3"
    , Text 26 "Bad news! The proton hit the earth, and everything will remake. (Press Resume to restart this level)"--level 3
    , Text 27 "Bad news! The proton hit the earth, and everything will remake. (Press Resume to restart this level)"--4
    , Text 28 "Oh no, the spacecraft escaped successfully! You lose one chance! (Press Resume to continue)"
    , Text 31 "Dear son: \nIt’s my last message."
    , Text 32 "You may not hear that, our empire has decided to escape from the sun, from our home to somewhere very remote."
    , Text 33 "Your final task, is still the same, in order to leave enough time for the rest of human beings to withdraw."
    , Text 34 "It’s an endless task, with much harder conditions. "
    , Text 35 "It’s hard to survive, but you must stay here, for everyone, for your heart and soul."
    , Text 36 "Welcome back, my child."
    , Text 37 "Your mission has been completed."
    , Text 38 "Congratulations! "
    , Text 39 "Your will live in our mind."
    , Text 40 "Made by Ocean Cat Studio\n Team member: Tingting Ding. Xu He. JOVEN LAURENS NICHOLAS,Yuewen Hou"
    , Text 41 "Apollo, by Ocean Cat Studio\n--Level 4"
    , Text 42 "Oh no, the spacecraft escaped successfully! You lose one chance! (Press Resume to continue)"
    ]