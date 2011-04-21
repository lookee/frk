-module(palettehsv).
-author('LucaAmore').
-export([color/2]).

color(Max,Max) ->
    R = 0,
    G = 0,
    B = 0,
    {R, G, B};

color(I,_) ->
    H = round (360 * math:log(I)) rem 360,
    S = 0.4,
    V = 0.5,
    {R, G, B} = hsv2rgb:convert(H,S,V),
    {round(R * 255), round(G * 255), round(B * 255)}.

