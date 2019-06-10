module Input



let pointsAndVectors =
  [ (-10351, -10360), ( 1,  1)
  ; ( 52528,  31539), (-5, -3)
  ; (-31270, -20838), ( 3,  2)
  ; ( 52486, -10365), (-5,  1)
  ; ( 31558,  10589), (-3, -1)
  ; (-52253,  21064), ( 5, -2)
  ; (-10354,  42015), ( 1, -4)
  ; (-41798,  42013), ( 4, -4)
  ; (-52253, -52267), ( 5,  5)
  ; ( 31550, -41793), (-3,  4)
  ; (-31290,  10591), ( 3, -1)
  ; ( 31542, -10363), (-3,  1)
  ; ( 21117,  52487), (-2, -5)
  ; ( 21074, -41796), (-2,  4)
  ; ( 10619, -20840), (-1,  2)
  ; ( 31562,  52495), (-3, -5)
  ; ( 31586, -20844), (-3,  2)
  ; (-20837,  42020), ( 2, -4)
  ; ( 52486,  10589), (-5, -1)
  ; ( 52518, -31313), (-5,  3)
  ; (-31286,  21063), ( 3, -2)
  ; ( 31536, -41793), (-3,  4)
  ; ( 52523, -52268), (-5,  5)
  ] @
  [ (-20830, -10364), ( 2,  1)
  ; ( 31568,  10587), (-3, -1)
  ; ( 21116, -10369), (-2,  1)
  ; ( 31558,  21060), (-3, -2)
  ; ( 21074,  42013), (-2, -4)
  ; ( 21114,  42015), (-2, -4)
  ; (-52231, -52269), ( 5,  5)
  ; (-20819,  21064), ( 2, -2)
  ; ( 42062, -10365), (-4,  1)
  ; (-31271,  52496), ( 3, -5)
  ; ( 10598, -41791), (-1,  4)
  ; ( 42038, -10361), (-4,  1)
  ; ( 42047, -52273), (-4,  5)
  ; (-31290,  10591), ( 3, -1)
  ; (-41742,  21066), ( 4, -2)
  ; ( 31590, -41790), (-3,  4)
  ; (-31314, -10360), ( 3,  1)
  ; ( 42011,  42011), (-4, -4)
  ; (-10366,  10583), ( 1, -1)
  ; (-10349, -20844), ( 1,  2)
  ; (-20806, -41790), ( 2,  4)
  ; ( 42018,  52494), (-4, -5)
  ; (-41746, -52271), ( 4,  5)
  ; ( 52507,  10590), (-5, -1)
  ; (-20793,  52487), ( 2, -5)
  ; ( 21106,  21067), (-2, -2)
  ; ( 42047,  21063), (-4, -2)
  ; (-20794, -31321), ( 2,  3)
  ; ( 10611, -20838), (-1,  2)
  ; (-41742, -41793), ( 4,  4)
  ; (-41747, -31321), ( 4,  3)
  ; (-20838, -10367), ( 2,  1)
  ; (-52274, -31314), ( 5,  3)
  ; ( 52526, -20841), (-5,  2)
  ] @
  [ ( 21066,  42017), (-2, -4)
  ; (-10354, -41789), ( 1,  4)
  ; (-10326,  31535), ( 1, -3)
  ; ( 42062, -20837), (-4,  2)
  ; (-10346,  31539), ( 1, -3)
  ; ( 21090,  52494), (-2, -5)
  ; (-20829, -41797), ( 2,  4)
  ; ( 21077,  21068), (-2, -2)
  ; (-20814, -20845), ( 2,  2)
  ; ( 21103, -10366), (-2,  1)
  ; (-52250, -52266), ( 5,  5)
  ; (-10341,  31537), ( 1, -3)
  ; ( 10603, -20839), (-1,  2)
  ; (-41782, -20839), ( 4,  2)
  ; (-52226, -41790), ( 5,  4)
  ; ( 31586, -41791), (-3,  4)
  ; (-20790,  31543), ( 2, -3)
  ; ( 31575, -41793), (-3,  4)
  ; ( 52490,  42011), (-5, -4)
  ; (-41741,  31535), ( 4, -3)
  ; (-10318, -31316), ( 1,  3)
  ; ( 10585, -10369), (-1,  1)
  ; ( 10606,  10591), (-1, -1)
  ; ( 52510,  31544), (-5, -3)
  ; (-20846, -20840), ( 2,  2)
  ; ( 21058,  52490), (-2, -5)
  ; (-10367, -31321), ( 1,  3)
  ; (-41774,  21062), ( 4, -2)
  ; (-52254,  52492), ( 5, -5)
  ; ( 10640, -10360), (-1,  1)
  ; (-20846, -20839), ( 2,  2)
  ; (-10341, -52272), ( 1,  5)
  ; (-10330, -10363), ( 1,  1)
  ; (-10353, -41797), ( 1,  4)
  ; (-41782,  52494), ( 4, -5)
  ] @
  [ (-41761,  10584), ( 4, -1)
  ; ( 42018, -10369), (-4,  1)
  ; (-10365,  42014), ( 1, -4)
  ; (-10318,  42018), ( 1, -4)
  ; (-52242, -31315), ( 5,  3)
  ; ( 21095, -20838), (-2,  2)
  ; ( 21058, -10368), (-2,  1)
  ; (-31282,  10584), ( 3, -1)
  ; (-31322, -31321), ( 3,  3)
  ; ( 21117, -52273), (-2,  5)
  ; ( 31543,  52496), (-3, -5)
  ; ( 52538,  21066), (-5, -2)
  ; ( 10582,  52492), (-1, -5)
  ; (-41777, -52265), ( 4,  5)
  ; (-10357, -52264), ( 1,  5)
  ; ( 42053,  31535), (-4, -3)
  ; ( 42028, -20845), (-4,  2)
  ; (-10326, -20845), ( 1,  2)
  ; ( 10638,  10585), (-1, -1)
  ; ( 31591,  42020), (-3, -4)
  ; ( 52505, -52273), (-5,  5)
  ; (-10338,  21059), ( 1, -2)
  ; (-10341,  42012), ( 1, -4)
  ; ( 31566,  31544), (-3, -3)
  ; (-52215, -10360), ( 5,  1)
  ; ( 52526,  21060), (-5, -2)
  ; ( 10638,  10589), (-1, -1)
  ; ( 42034,  10584), (-4, -1)
  ; ( 31553, -52264), (-3,  5)
  ; (-10353, -10360), ( 1,  1)
  ; (-20795, -31321), ( 2,  3)
  ; (-20830,  42014), ( 2, -4)
  ; ( 10606, -31319), (-1,  3)
  ; (-20814, -41796), ( 2,  4)
  ] @
  [ (-52239,  31539), ( 5, -3)
  ; ( 31586,  10586), (-3, -1)
  ; (-10318,  31540), ( 1, -3)
  ; ( 10643,  10584), (-1, -1)
  ; ( 42042,  10592), (-4, -1)
  ; (-41749, -10360), ( 4,  1)
  ; (-41774,  52490), ( 4, -5)
  ; ( 31590,  31536), (-3, -3)
  ; (-10350, -41789), ( 1,  4)
  ; ( 42047, -52264), (-4,  5)
  ; ( 10631, -20836), (-1,  2)
  ; ( 52542, -52270), (-5,  5)
  ; (-52266,  21062), ( 5, -2)
  ; (-52258, -10363), ( 5,  1)
  ; (-41793,  42016), ( 4, -4)
  ; (-31277, -10367), ( 3,  1)
  ; ( 52510, -52270), (-5,  5)
  ; (-20842, -52269), ( 2,  5)
  ; ( 10630, -10362), (-1,  1)
  ; ( 21082,  42011), (-2, -4)
  ; ( 21062,  52487), (-2, -5)
  ; ( 52503,  31544), (-5, -3)
  ; ( 10587, -31318), (-1,  3)
  ; (-41766,  42015), ( 4, -4)
  ; (-52245, -52267), ( 5,  5)
  ; (-31277,  31538), ( 3, -3)
  ; (-10362,  10583), ( 1, -1)
  ; ( 31593,  52496), (-3, -5)
  ; ( 21108, -20836), (-2,  2)
  ; (-52266,  52490), ( 5, -5)
  ; ( 21094, -10365), (-2,  1)
  ; ( 31575, -20841), (-3,  2)
  ; (-31285,  42013), ( 3, -4)
  ; ( 10625,  10583), (-1, -1)
  ; (-10313, -52264), ( 1,  5)
  ; ( 42036,  42014), (-4, -4)
  ; ( 31561,  10589), (-3, -1)
  ; ( 10624, -41793), (-1,  4)
  ; (-52274, -31314), ( 5,  3)
  ; ( 10587, -10361), (-1,  1)
  ; ( 31545, -41788), (-3,  4)
  ; ( 21063, -20838), (-2,  2)
  ; (-52234, -41790), ( 5,  4)
  ; ( 52523,  52490), (-5, -5)
  ; ( 42047, -10361), (-4,  1)
  ; (-41774,  10587), ( 4, -1)
  ; (-52274,  42013), ( 5, -4)
  ; ( 21106,  52494), (-2, -5)
  ; ( 52546, -10360), (-5,  1)
  ] @
  [ ( 10633, -20836), (-1,  2)
  ; (-10370, -20844), ( 1,  2)
  ; ( 42030, -20837), (-4,  2)
  ; ( 21075,  42020), (-2, -4)
  ; (-20820,  21063), ( 2, -2)
  ; ( 10622,  21062), (-1, -2)
  ; ( 42066,  21065), (-4, -2)
  ; ( 52528, -52273), (-5,  5)
  ; ( 10610, -31313), (-1,  3)
  ; (-10370, -20837), ( 1,  2)
  ; (-20788,  31535), ( 2, -3)
  ; (-20844,  52487), ( 2, -5)
  ; ( 52510, -41788), (-5,  4)
  ; ( 31566, -20844), (-3,  2)
  ; ( 42047,  42012), (-4, -4)
  ; (-31282, -41789), ( 3,  4)
  ; (-41742,  52493), ( 4, -5)
  ; (-41758,  21061), ( 4, -2)
  ; ( 31579,  52489), (-3, -5)
  ; (-31317,  42013), ( 3, -4)
  ; ( 10611,  21059), (-1, -2)
  ; ( 31535, -41797), (-3,  4)
  ; (-10338, -10365), ( 1,  1)
  ; (-52261,  31544), ( 5, -3)
  ; (-10368, -10369), ( 1,  1)
  ; (-41746,  10587), ( 4, -1)
  ; ( 31571,  31535), (-3, -3)
  ; (-20786, -10360), ( 2,  1)
  ; (-52258,  10584), ( 5, -1)
  ; ( 52494, -31315), (-5,  3)
  ; (-31317,  52492), ( 3, -5)
  ; ( 31566, -20839), (-3,  2)
  ; (-20825, -52264), ( 2,  5)
  ; ( 10614, -10366), (-1,  1)
  ; (-31277, -10366), ( 3,  1)
  ; ( 52488,  42020), (-5, -4)
  ; (-41795,  31539), ( 4, -3)
  ; ( 52520, -52269), (-5,  5)
  ; ( 21070,  52496), (-2, -5)
  ; (-31293, -31315), ( 3,  3)
  ; (-10314,  42016), ( 1, -4)
  ; (-20814, -31319), ( 2,  3)
  ; (-41777, -52265), ( 4,  5)
  ; ( 21079,  52494), (-2, -5)
  ; ( 21101, -10369), (-2,  1)
  ; (-20821,  31537), ( 2, -3)
  ; (-20802,  21063), ( 2, -2)
  ; ( 52531,  21060), (-5, -2)
  ; (-20828,  52496), ( 2, -5)
  ; ( 10590,  52488), (-1, -5)
  ; ( 52511,  31537), (-5, -3)
  ; (-20817, -20839), ( 2,  2)
  ; ( 21082,  10587), (-2, -1)
  ; ( 52530, -31321), (-5,  3)
  ; (-41782, -41795), ( 4,  4)
  ; ( 42034, -10360), (-4,  1)
  ; (-41765, -20841), ( 4,  2)
  ; (-41788,  42020), ( 4, -4)
  ; ( 42047,  31544), (-4, -3)
  ; (-31314, -41793), ( 3,  4)
  ; ( 10611,  31543), (-1, -3)
  ; ( 10611, -10360), (-1,  1)
  ; ( 31542, -41795), (-3,  4)
  ; ( 42050, -52269), (-4,  5)
  ; ( 31592,  10592), (-3, -1)
  ; (-10317,  21059), ( 1, -2)
  ; ( 52544, -31321), (-5,  3)
  ] @
  [ (-31282,  52488), ( 3, -5)
  ; (-52242,  31535), ( 5, -3)
  ; (-31290, -52271), ( 3,  5)
  ; (-52256, -31312), ( 5,  3)
  ; (-31306, -10364), ( 3,  1)
  ; ( 21077, -20840), (-2,  2)
  ; ( 10598, -10365), (-1,  1)
  ; (-41761, -20837), ( 4,  2)
  ; ( 31571, -31315), (-3,  3)
  ; (-41772,  10587), ( 4, -1)
  ; (-41750,  21067), ( 4, -2)
  ; ( 31539, -31314), (-3,  3)
  ; ( 10619,  10590), (-1, -1)
  ; (-10341,  31539), ( 1, -3)
  ; (-20814, -52268), ( 2,  5)
  ; (-20814,  21066), ( 2, -2)
  ; ( 10614,  10585), (-1, -1)
  ; (-41774,  21061), ( 4, -2)
  ; ( 10622, -20840), (-1,  2)
  ; (-41793,  31541), ( 4, -3)
  ; (-20841,  21061), ( 2, -2)
  ; ( 10622, -41792), (-1,  4)
  ; (-31317,  10590), ( 3, -1)
  ; (-10341, -10369), ( 1,  1)
  ; (-31322,  42018), ( 3, -4)
  ; (-20838, -20840), ( 2,  2)
  ; (-20844, -10360), ( 2,  1)
  ; ( 10634, -41789), (-1,  4)
  ; ( 42010,  52496), (-4, -5)
  ; (-41777,  31542), ( 4, -3)
  ; ( 10602, -31316), (-1,  3)
  ; ( 52538,  42015), (-5, -4)
  ; ( 21083,  52489), (-2, -5)
  ; ( 21066,  21060), (-2, -2)
  ; (-20805, -41793), ( 2,  4)
  ; ( 21087, -41788), (-2,  4)
  ; ( 52514,  21066), (-5, -2)
  ; ( 52515, -41790), (-5,  4)
  ; ( 21066, -31313), (-2,  3)
  ; (-41753, -52272), ( 4,  5)
  ; ( 10587,  31537), (-1, -3)
  ; (-20814, -31312), ( 2,  3)
  ; ( 10627,  52488), (-1, -5)
  ; ( 52523, -20837), (-5,  2)
  ] @
  [ (-41774,  52492), ( 4, -5)
  ; (-52269,  21062), ( 5, -2)
  ; (-31282,  10583), ( 3, -1)
  ; (-31322,  21059), ( 3, -2)
  ; (-31302, -20845), ( 3,  2)
  ; ( 52526, -10360), (-5,  1)
  ; ( 31536, -41793), (-3,  4)
  ; (-20809,  42015), ( 2, -4)
  ; ( 31542, -52271), (-3,  5)
  ; ( 52486, -10363), (-5,  1)
  ; (-10311,  42011), ( 1, -4)
  ; (-41761,  10584), ( 4, -1)
  ; ( 42050, -41789), (-4,  4)
  ; ( 52515, -20838), (-5,  2)
  ; ( 21058, -20841), (-2,  2)
  ; ( 31586, -31320), (-3,  3)
  ; (-41766,  31541), ( 4, -3)
  ; (-41769,  31540), ( 4, -3)
  ; (-52250, -31320), ( 5,  3)
  ; ( 52543,  42020), (-5, -4)
  ; ( 42052, -20841), (-4,  2)
  ; ( 52499,  31544), (-5, -3)
  ; ( 52515, -20842), (-5,  2)
  ; ( 31537,  31535), (-3, -3)
  ; ( 52490, -20836), (-5,  2)
  ; ( 52518,  21066), (-5, -2)
  ; ( 31590,  52492), (-3, -5)
  ; (-10310, -10369), ( 1,  1)
  ; ( 31582,  42019), (-3, -4)
  ; (-10368, -52269), ( 1,  5)
  ; ( 31539, -20844), (-3,  2)
  ; ( 10590, -41796), (-1,  4)
  ; ( 52538, -20837), (-5,  2)
  ; (-41737,  31543), ( 4, -3)
  ; (-52271, -31312), ( 5,  3)
  ; ( 31559,  42012), (-3, -4)
  ; (-10330,  31542), ( 1, -3)
  ; (-41797,  31539), ( 4, -3)
  ; ( 10639,  52487), (-1, -5)
  ; ( 31566, -41792), (-3,  4)
  ; ( 42068,  42020), (-4, -4)
  ; ( 42066, -20840), (-4,  2)
  ; ( 10583, -52264), (-1,  5)
  ; ( 21066,  31543), (-2, -3)
  ; ( 10606, -41790), (-1,  4)
  ; ( 42042, -41793), (-4,  4)
  ; ( 42038, -31314), (-4,  3)
  ; (-20806, -20842), ( 2,  2)
  ; ( 42010, -10366), (-4,  1)
  ; (-41795, -20841), ( 4,  2)
  ; ( 10633,  21059), (-1, -2)
  ; ( 52527, -10369), (-5,  1)
  ; ( 21109, -31312), (-2,  3)
  ; (-52224,  52496), ( 5, -5)
  ; ( 10583,  10592), (-1, -1)
  ]
