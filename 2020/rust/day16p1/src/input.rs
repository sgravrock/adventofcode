pub fn puzzle_input() -> &'static str {
	"departure location: 41-525 or 538-968
departure station: 50-380 or 395-960
departure platform: 25-507 or 521-953
departure track: 41-401 or 411-953
departure date: 36-274 or 300-970
departure time: 33-739 or 748-959
arrival location: 42-103 or 110-968
arrival station: 36-417 or 438-967
arrival platform: 26-905 or 929-954
arrival track: 49-153 or 163-954
class: 45-710 or 736-970
duration: 47-804 or 827-960
price: 48-250 or 259-970
route: 43-640 or 666-958
row: 32-474 or 493-969
seat: 31-876 or 889-970
train: 49-754 or 762-972
type: 31-761 or 782-963
wagon: 32-455 or 461-956
zone: 39-612 or 629-972

your ticket:
53,101,83,151,127,131,103,61,73,71,97,89,113,67,149,163,139,59,79,137

nearby tickets:
874,219,523,563,260,900,866,351,171,669,504,97,415,22,893,605,395,376,200,600
450,692,869,242,163,249,857,666,165,268,236,416,603,933,172,500,745,227,110,858
538,86,168,938,68,112,899,232,835,844,15,359,593,133,130,876,588,930,196,873
125,831,578,67,948,60,89,274,547,803,217,460,268,181,74,447,453,838,201,688
539,592,346,599,57,793,337,102,666,934,875,153,801,467,842,346,542,797,881,118
239,685,612,460,361,86,197,737,325,794,181,443,217,372,90,63,399,356,870,230
125,668,464,748,273,363,993,610,834,939,736,374,870,247,789,543,353,938,399,218
558,110,495,23,202,358,170,737,313,216,320,355,589,700,666,373,199,505,570,836
671,784,347,946,173,70,103,710,348,123,203,446,77,600,576,709,883,217,179,149
126,904,310,635,847,363,741,601,86,327,66,801,800,671,127,832,194,91,416,836
694,330,947,838,413,285,561,60,671,438,693,871,93,71,694,147,668,525,86,165
303,469,143,609,948,308,470,8,60,211,183,788,903,182,707,225,501,668,896,150
179,133,55,782,445,684,263,506,545,414,671,95,876,947,467,354,814,790,327,197
318,604,946,494,335,707,363,361,498,237,207,326,400,682,350,457,547,243,698,304
560,854,503,723,225,323,442,839,553,213,840,548,677,309,752,327,414,577,587,50
84,841,342,498,439,557,785,747,471,183,859,348,630,249,243,67,611,946,174,558
225,552,72,268,703,904,256,935,830,595,610,904,73,470,240,504,243,597,148,472
264,305,312,149,60,302,574,373,600,380,354,70,171,805,326,791,190,573,859,593
128,694,557,111,362,597,937,113,577,462,462,785,910,671,186,859,132,96,64,583
553,524,859,306,196,861,197,144,676,449,834,540,598,997,414,202,639,375,708,273
319,176,322,547,579,581,349,416,846,362,326,334,322,357,895,195,739,882,242,103
868,462,227,547,597,873,752,633,792,264,5,548,365,700,323,830,596,453,144,748
865,691,605,752,136,820,835,674,375,116,539,841,202,141,310,608,707,348,112,593
462,354,262,902,913,631,318,73,372,152,842,367,118,547,680,598,561,379,565,543
944,582,800,793,214,85,678,629,308,666,702,11,860,304,854,548,61,500,581,941
863,240,417,584,129,930,172,862,578,196,676,923,463,670,506,739,181,541,153,184
21,578,209,102,578,113,938,605,612,184,782,68,840,440,361,262,356,368,259,103
505,799,447,351,507,193,242,450,205,786,308,11,343,120,378,560,893,455,202,342
683,206,813,789,851,373,245,462,639,319,695,122,369,373,137,224,857,640,272,243
188,263,121,261,896,737,482,247,238,217,870,332,591,540,804,415,600,465,793,894
68,345,462,253,86,602,190,208,344,693,704,222,602,671,842,563,77,607,264,631
118,353,560,180,346,197,209,77,88,182,503,855,944,232,596,528,784,66,266,796
837,373,702,907,934,88,594,121,791,702,548,786,441,895,191,692,305,739,144,230
169,879,699,844,217,332,563,169,169,697,554,596,97,858,411,894,667,339,736,372
845,904,553,247,986,557,465,111,851,903,837,346,210,687,610,635,121,304,872,581
603,804,448,849,892,895,270,351,683,569,352,889,797,76,897,228,472,241,332,433
890,152,85,402,444,309,785,751,170,237,247,351,304,895,827,827,461,135,268,505
438,835,93,513,540,696,581,844,467,856,595,241,362,834,891,929,171,829,246,708
528,351,931,894,591,667,904,637,209,594,416,539,324,188,90,124,51,636,76,847
570,191,606,585,598,191,872,101,110,501,503,847,522,702,860,844,287,136,196,79
794,169,707,445,326,273,321,978,414,754,568,322,675,54,454,202,506,857,571,377
228,371,259,203,174,591,326,245,334,568,760,546,66,193,201,230,377,897,232,150
847,451,601,695,798,88,133,289,565,355,56,607,830,595,306,446,541,397,565,839
584,203,410,794,679,417,266,401,250,357,170,788,271,50,67,834,414,702,213,835
704,601,227,609,344,417,539,662,401,343,710,754,859,470,212,525,316,325,591,142
227,298,848,93,339,566,71,220,795,599,316,365,830,841,309,441,129,937,180,667
439,172,557,191,345,174,602,608,214,117,61,139,754,524,348,903,348,743,204,376
982,933,698,354,350,116,594,167,145,592,100,94,683,65,202,610,838,688,346,947
115,50,362,317,307,221,978,326,243,678,359,575,596,901,709,904,439,372,575,893
752,323,127,183,589,572,503,673,700,194,172,844,61,353,170,354,4,200,638,132
546,353,78,218,234,847,349,941,832,461,204,204,412,758,259,138,791,606,228,365
519,575,612,204,359,378,364,225,124,100,80,243,130,896,91,92,604,55,111,938
288,608,325,211,416,858,52,703,451,209,77,890,440,209,95,90,206,165,525,359
86,710,788,335,450,325,214,309,496,707,392,448,132,497,185,300,469,173,684,203
667,819,358,804,100,889,496,556,118,114,673,371,603,503,68,931,55,65,941,269
489,194,691,471,137,186,190,580,50,637,312,219,500,889,54,192,262,439,943,138
561,495,139,262,791,837,919,340,900,84,207,323,375,114,501,941,216,68,893,310
500,684,559,558,223,756,829,849,171,205,61,566,864,346,844,57,496,69,413,611
736,68,412,594,551,92,72,750,506,549,524,208,782,439,153,673,300,541,489,65
318,495,547,860,893,7,445,329,77,103,857,338,351,221,794,579,151,210,176,190
948,611,749,605,83,850,112,605,356,840,413,687,79,181,913,75,934,706,692,873
313,213,438,618,707,934,131,145,583,177,751,166,376,230,148,131,152,320,317,132
904,397,324,51,690,380,269,845,266,359,368,228,415,845,91,498,400,119,319,284
889,331,144,374,802,325,587,827,559,54,116,357,631,302,78,823,636,86,311,362
702,834,152,189,444,138,394,935,559,273,196,375,574,301,265,58,845,242,260,204
738,798,84,230,240,677,493,140,98,578,186,870,667,218,633,832,269,313,911,79
885,702,342,696,673,494,189,163,831,69,123,368,899,123,667,873,738,338,588,143
661,219,230,680,454,847,872,542,585,173,680,300,792,183,553,204,943,198,869,55
307,214,179,607,77,62,566,370,857,686,345,670,357,60,111,848,209,471,988,311
940,896,441,5,309,80,901,121,115,579,221,270,143,369,368,306,936,550,348,139
697,193,226,557,468,497,220,892,237,203,188,214,494,940,866,859,171,257,704,103
222,470,750,359,126,752,226,873,874,415,51,258,110,316,901,54,91,561,945,893
785,163,572,287,240,245,599,546,440,631,215,170,750,417,362,674,633,668,327,561
274,222,99,582,786,53,62,116,262,603,742,374,843,397,171,440,938,451,671,844
546,692,684,374,112,607,72,605,495,505,124,120,603,197,335,375,455,328,6,843
183,922,875,171,175,841,524,142,446,442,753,861,70,904,893,842,842,804,169,210
346,446,83,537,801,498,795,707,265,264,570,630,83,752,681,506,175,417,60,750
185,522,737,855,441,830,543,259,56,686,892,113,78,344,401,405,163,145,89,141
549,182,559,469,336,367,71,749,397,96,208,581,52,791,977,250,611,840,945,320
457,563,837,554,94,895,847,688,864,583,462,546,585,903,309,902,793,235,199,198
782,145,343,802,669,306,585,467,942,550,334,678,61,193,65,701,683,524,841,526
334,191,984,178,895,231,304,222,539,525,678,837,584,553,890,367,327,355,699,250
86,328,498,362,873,119,221,232,218,473,216,128,689,502,246,695,205,130,302,394
242,360,343,574,461,335,704,198,802,79,179,248,261,843,675,378,934,553,703,404
799,597,752,986,467,438,51,868,698,220,444,58,214,603,904,263,207,417,94,61
699,687,863,411,101,612,507,567,270,331,737,749,445,401,124,560,256,668,54,799
597,365,376,442,113,566,113,547,399,936,235,380,248,58,351,78,509,395,788,847
203,894,595,192,671,56,120,396,74,474,167,363,100,595,865,8,224,236,593,374
503,939,685,557,346,930,374,167,238,102,274,539,602,152,566,978,895,122,669,844
943,830,602,561,316,69,940,177,801,697,446,937,450,885,852,324,202,608,903,691
71,590,398,873,313,635,553,670,944,306,608,267,556,253,226,167,315,738,609,867
811,309,100,546,81,832,167,164,216,548,601,398,559,126,357,191,754,944,73,794
380,237,937,584,497,205,608,408,79,703,263,90,263,400,670,146,330,184,51,680
208,594,357,859,201,201,344,945,164,184,352,180,680,670,241,917,705,890,788,572
99,203,400,61,695,601,588,838,234,311,795,783,682,88,250,990,503,327,704,352
556,306,449,633,265,190,302,792,243,73,850,348,593,51,231,874,174,478,670,677
306,500,361,631,708,271,402,218,564,237,130,86,365,736,944,319,330,141,139,835
890,503,847,165,326,466,78,141,797,855,834,221,978,691,896,83,463,125,574,752
135,596,587,940,206,852,107,190,795,554,845,522,238,670,337,320,848,561,233,181
899,786,177,323,466,669,57,458,451,524,336,591,684,935,595,929,677,178,506,683
354,803,896,564,362,269,861,854,828,195,630,65,375,264,889,603,325,237,456,942
265,372,444,900,602,338,932,200,507,259,694,563,544,464,150,365,759,273,454,602
184,172,132,553,90,496,300,194,919,78,316,555,937,94,572,502,834,243,844,802
77,804,183,680,268,129,281,682,165,800,123,524,377,900,683,798,202,687,835,454
891,849,378,503,691,899,440,415,787,447,580,862,50,874,795,432,894,684,98,317
705,831,873,679,976,119,224,345,344,127,473,581,439,704,62,332,706,890,144,495
370,359,552,608,439,939,216,107,736,874,539,129,271,370,943,261,217,329,595,830
687,274,519,324,194,318,495,168,702,832,304,342,874,318,681,164,343,329,170,786
899,451,135,348,749,149,210,714,89,896,848,904,116,331,942,605,359,469,320,864
77,784,849,305,896,255,796,797,598,596,78,864,939,453,145,53,852,843,895,231
841,706,753,163,542,574,609,202,545,57,683,355,542,224,548,234,498,914,235,330
565,595,365,60,69,693,493,700,898,121,308,815,502,210,446,141,123,752,237,92
90,309,630,200,791,80,594,143,95,854,795,673,983,748,453,212,136,443,180,858
672,111,246,100,608,337,759,300,216,167,837,63,563,198,793,301,307,785,181,538
134,578,55,669,602,303,464,313,786,932,227,783,76,793,799,310,920,366,525,327
670,892,180,401,235,886,799,582,73,599,787,81,327,704,892,693,575,754,591,901
159,60,842,633,212,138,337,797,259,858,525,142,899,500,737,219,754,328,305,840
571,738,564,901,561,225,880,74,322,499,147,101,53,220,465,752,66,446,302,739
62,871,352,167,324,930,305,669,673,561,182,800,709,590,304,710,203,756,606,450
324,582,62,133,865,237,246,811,441,273,522,269,685,463,116,571,376,350,472,698
268,317,742,182,415,314,376,930,140,180,372,682,687,126,674,71,850,543,837,561
572,142,327,61,202,676,119,919,599,448,589,795,339,902,671,676,944,847,853,267
561,629,257,302,199,333,374,866,844,637,142,88,412,751,748,684,792,676,547,91
449,942,872,830,379,84,193,477,372,897,861,417,690,314,931,782,123,79,438,272
939,700,931,673,125,640,213,594,333,260,698,229,235,334,316,636,783,192,861,821
83,569,375,323,363,555,782,885,147,399,225,538,260,749,224,249,67,467,700,752
72,871,837,800,357,340,441,549,804,400,281,842,681,61,860,82,550,141,452,629
689,212,846,697,333,546,74,61,362,596,408,564,611,853,558,467,308,397,234,564
370,358,284,609,704,417,876,543,90,326,440,705,604,474,542,189,788,113,549,579
97,186,569,560,445,835,706,391,468,346,220,330,572,102,147,904,360,304,199,331
586,223,223,365,848,876,173,379,782,183,245,354,876,949,182,24,602,828,693,689
203,309,785,495,577,374,183,939,200,470,181,207,350,152,739,500,690,319,94,759
208,949,242,338,348,476,876,564,633,63,791,689,860,502,945,636,599,268,859,789
554,841,862,218,885,153,708,194,138,843,738,575,211,65,506,544,630,674,870,356
66,102,866,204,831,371,937,324,174,931,470,573,845,580,99,309,195,147,586,728
219,502,752,856,130,87,841,761,607,334,553,630,690,412,376,640,499,98,749,859
325,411,700,828,311,55,580,89,782,467,398,551,145,368,697,101,491,859,178,543
211,331,557,56,227,525,82,188,190,750,467,796,875,152,999,503,749,71,566,324
384,793,828,463,166,751,110,168,312,800,899,314,860,681,789,899,89,860,674,181
244,338,268,888,97,500,345,905,235,474,100,633,736,494,232,76,362,899,584,414
523,743,348,591,788,669,197,262,471,570,99,115,681,151,598,372,214,865,869,94
672,202,91,305,310,803,667,119,669,939,872,560,872,699,887,60,543,242,412,558
106,672,701,604,782,242,199,247,193,604,120,351,841,501,690,701,349,374,120,789
582,194,346,680,549,852,697,325,500,569,350,539,670,590,63,257,50,461,752,502
868,464,457,893,96,312,198,204,57,316,205,101,902,448,399,67,831,452,599,345
756,341,306,846,219,209,75,398,204,863,211,221,448,560,639,331,88,192,635,522
631,208,121,136,438,224,506,53,738,222,689,197,591,64,932,267,540,752,903,477
875,355,495,756,522,331,938,183,578,473,893,210,60,261,231,889,334,472,130,334
942,836,328,261,142,937,809,304,563,363,203,850,191,116,668,128,889,199,471,782
171,96,317,266,737,946,456,237,831,833,90,222,601,117,65,592,219,523,188,79
448,480,411,783,274,872,783,395,796,751,572,634,553,551,672,589,55,686,902,840
169,687,127,193,465,930,666,500,268,239,677,860,171,122,447,749,849,185,841,740
682,789,540,707,552,541,750,594,853,601,442,507,215,233,500,757,855,555,572,831
862,193,191,314,674,944,833,174,592,599,468,320,560,443,561,130,218,120,521,516
225,150,500,801,936,205,473,622,588,789,599,554,60,152,564,82,543,559,58,669
625,604,99,933,941,849,195,599,464,89,75,548,400,259,135,582,893,583,560,176
797,525,205,700,164,361,546,840,786,351,270,121,792,339,630,850,681,786,861,460
86,758,260,640,353,750,464,859,749,219,166,356,841,151,896,472,98,223,584,239
684,568,126,463,848,180,103,521,842,271,807,832,605,58,788,271,333,339,223,686
750,222,905,490,576,201,144,708,85,169,365,797,94,748,77,204,700,126,82,748
911,140,346,301,134,110,97,328,141,197,748,414,443,553,842,582,507,612,355,263
145,506,316,135,313,374,546,78,891,124,95,55,325,457,541,447,147,692,371,321
320,901,225,937,703,9,873,468,840,589,751,585,368,842,241,800,873,854,905,137
268,582,564,842,77,601,82,856,999,561,503,396,580,300,146,860,347,461,706,68
165,507,138,790,138,876,846,231,235,853,113,66,631,353,411,303,542,634,141,11
76,938,216,100,595,748,243,601,573,787,355,362,498,576,543,509,395,674,606,438
334,459,347,133,175,830,589,121,127,672,113,493,586,701,675,681,113,609,561,180
667,144,338,213,56,196,217,692,467,350,692,463,147,249,196,399,548,324,893,877
871,85,841,331,612,708,376,231,343,351,559,447,887,64,182,500,314,603,83,196
83,371,267,376,782,380,110,92,704,302,566,330,453,340,698,204,852,261,975,96
855,929,357,809,99,797,196,73,318,945,247,304,804,470,801,634,666,189,841,413
185,462,319,14,121,506,73,710,466,471,472,541,942,451,471,164,174,82,69,143
337,314,603,608,77,339,692,936,564,317,252,442,850,62,168,442,446,170,59,827
426,931,782,81,684,898,413,185,247,469,591,171,60,357,86,830,93,319,103,262
318,401,864,74,354,451,527,151,832,634,180,376,128,401,79,102,677,605,539,310
56,934,68,268,850,460,684,554,416,676,686,793,792,635,207,323,788,121,224,894
124,218,848,753,260,199,115,304,213,875,853,842,201,570,479,137,585,571,782,191
68,450,704,936,414,261,168,605,590,596,608,750,612,572,456,184,584,193,119,376
493,493,59,795,72,264,323,551,230,694,466,344,900,580,203,69,499,555,87,255
263,323,324,895,209,331,901,633,589,948,586,977,240,325,786,843,401,219,212,147
678,93,239,217,136,674,170,165,270,559,201,119,79,584,809,560,198,896,94,860
111,556,340,55,24,220,673,324,191,946,839,274,629,112,784,736,569,411,346,370
405,351,415,633,863,558,222,828,370,785,120,69,143,360,893,62,449,454,892,174
595,989,632,938,552,785,207,500,96,184,261,355,226,749,137,127,462,783,237,369
53,114,228,263,901,830,125,545,217,932,24,248,889,103,74,896,222,692,794,506
474,185,211,59,576,223,684,74,538,865,147,365,170,20,351,585,702,446,549,146
326,255,683,174,376,689,95,332,174,871,232,191,124,470,580,51,540,590,855,55
134,59,871,265,581,874,542,269,570,171,477,929,692,378,201,692,245,791,50,324
304,348,607,133,975,414,672,51,194,95,688,596,325,569,705,328,130,858,841,117
310,145,758,666,668,544,804,872,790,454,522,583,469,802,164,244,894,684,523,439
836,228,555,452,473,674,139,679,702,702,680,934,541,482,552,373,417,861,829,692
473,931,635,398,313,603,546,550,446,708,666,754,936,445,263,879,150,868,677,568
859,893,347,420,69,447,547,892,143,72,788,639,152,148,564,844,163,181,397,246
135,559,259,138,604,326,339,527,679,690,401,929,310,542,929,303,590,828,265,237
678,401,503,468,590,148,630,443,453,863,350,782,119,84,597,179,302,375,785,106
857,452,612,449,416,666,187,582,218,683,877,675,469,547,684,540,376,242,355,675
723,562,608,708,799,897,100,449,339,945,579,859,555,607,272,365,151,173,120,547
333,830,377,385,199,539,545,59,450,450,834,496,123,189,149,53,367,60,249,525
213,472,75,848,349,507,182,309,699,147,790,600,444,564,505,223,637,818,473,943
695,367,782,576,604,352,377,143,88,938,541,491,590,65,629,538,259,164,830,452
70,194,61,205,898,671,453,56,316,137,583,364,221,523,96,385,866,246,128,307
629,739,93,190,399,924,222,227,753,689,142,152,199,636,309,354,350,801,343,359
395,339,465,631,51,307,560,518,177,556,335,690,865,235,305,595,581,838,498,600
471,833,632,444,679,577,694,361,802,56,245,186,590,121,523,349,459,581,85,95
593,210,475,801,194,870,706,441,143,751,215,342,232,791,189,867,249,223,359,931
195,240,111,868,931,584,6,377,863,863,240,120,865,319,112,242,75,265,469,507
439,270,569,240,163,789,376,466,905,439,569,346,674,117,129,893,993,830,897,556
752,89,842,500,890,567,77,369,867,416,603,889,751,587,118,995,376,309,184,143
741,50,76,572,608,539,196,521,854,872,354,946,199,400,135,468,416,274,200,578
552,495,849,308,795,249,565,212,345,122,397,395,851,939,902,996,217,354,229,540
439,846,803,201,794,935,367,565,97,396,76,180,316,192,608,279,375,557,674,144
694,540,99,832,785,743,342,635,302,357,932,466,185,347,790,114,556,140,121,471
696,942,176,338,249,447,64,18,603,181,789,260,538,701,850,366,327,791,65,800
121,572,877,344,72,583,103,751,170,804,55,52,948,672,186,505,839,749,274,599
251,854,64,53,445,634,171,316,793,524,137,577,552,736,373,62,401,466,606,67
77,51,835,679,538,174,636,470,461,461,352,332,200,18,698,898,304,754,102,522
678,137,320,313,366,68,946,504,198,55,858,169,233,581,75,345,499,347,262,915
538,118,168,494,938,362,260,943,64,640,928,371,451,225,398,356,400,551,889,368
603,605,706,51,791,600,670,142,569,667,611,306,399,202,262,486,84,186,222,562
117,325,242,174,676,234,308,710,122,845,445,165,697,917,129,789,379,890,936,126
750,631,245,568,99,559,375,202,325,271,582,187,629,328,698,736,839,460,167,231
440,676,219,82,380,587,90,81,96,69,234,888,117,103,784,670,673,203,99,933
398,845,82,371,471,690,604,225,631,142,784,875,692,304,601,745,569,350,835,373
503,371,469,138,235,94,511,137,608,123,359,348,897,705,227,804,593,539,182,633
136,328,795,834,455,890,525,322,863,638,458,829,194,66,235,74,844,541,74,167
180,947,128,612,459,165,704,194,942,114,142,610,446,607,851,750,396,121,83,602
215,844,334,743,53,146,874,633,629,448,670,589,672,567,902,838,905,411,131,875
867,738,333,828,411,785,554,18,691,65,302,113,795,839,116,142,467,736,134,178
61,170,588,304,558,220,338,543,497,376,527,473,314,100,600,849,248,242,944,367
891,502,872,87,273,790,895,640,313,73,509,453,930,306,667,322,589,937,449,523
497,470,523,105,373,840,605,70,113,361,876,330,798,786,208,472,705,271,102,551
86,455,923,337,93,313,444,861,319,846,565,681,572,165,151,828,336,116,867,837
182,351,176,524,707,705,675,423,189,596,77,895,308,867,328,263,590,137,71,315
265,395,180,690,415,208,80,254,379,693,70,186,554,846,118,521,212,319,58,126
549,247,87,265,329,941,427,690,368,553,504,787,312,689,566,612,571,694,174,416
69,595,790,589,51,300,452,178,797,856,417,855,201,150,85,900,403,850,570,902
119,936,449,9,230,147,736,110,831,70,135,92,75,633,707,597,313,539,702,304
858,682,750,332,359,514,181,597,343,933,135,264,636,939,466,339,904,346,57,866
899,232,536,52,60,313,139,461,117,271,143,302,72,401,172,200,348,558,525,220
211,554,234,864,601,573,942,354,370,639,598,560,981,138,753,709,790,672,570,145
463,173,86,198,987,51,549,567,737,690,170,340,188,496,319,598,600,61,214,203"
}
