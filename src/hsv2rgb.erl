-module(hsv2rgb).
-author('LucaAmore').
-export([convert/3]).

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

% http://en.wikipedia.org/wiki/HSL_and_HSV#Converting_to_RGB
% H [0,360], S [0,1], V [0,1]
% R [0,1], G [0,1], B [0,1]

convert(H,S,V)->
    C = V * S,
    Hi = H div 60,
    X = C * (1 - abs(Hi rem 2 - 1)),
    case Hi of
        0 -> Ri = C, Gi = X, Bi = 0;
        1 -> Ri = X, Gi = C, Bi = 0;
        2 -> Ri = 0, Gi = C, Bi = X;
        3 -> Ri = 0, Gi = X, Bi = C;
        4 -> Ri = X, Gi = X, Bi = C;
        5 -> Ri = C, Gi = X, Bi = X
    end,
    M = V - C,
    {Ri + M, Gi + M, Bi + M}.
