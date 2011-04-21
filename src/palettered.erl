-module(palettered).
-author('LucaAmore').
-export([color/2]).

%------------------------------------------------------------------------
%    Copyright (C) 2011 Luca Amore <luca.amore at gmail.com>
%
%    frk is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    frk is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with frk.  If not, see <http://www.gnu.org/licenses/>.
%------------------------------------------------------------------------

color(Max,Max) ->
    R = 0,
    G = 0,
    B = 0,
    {R, G, B};

color(I,Max) when I > Max/2 ->
    K = I - Max/2,
    R = round(                            250),
    G = round((255 -  30 ) * 2 * K/Max  +   0),
    B = round(( 50 -  30 ) * 2 * K/Max  +  30),
    {R, G, B};

color(I,Max) when I =< Max/2 ->
    R = round(( 250 - 50 ) * 2 * I/Max  +  50),
    G = round(( 30  - 10 ) * 2 * I/Max  +  10),
    B = round(( 30  - 10 ) * 2 * I/Max  +  10),
    {R, G, B}.
