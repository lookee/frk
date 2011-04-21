-module(frk).
-author('LucaAmore').
-export([save/11, save/9, julia/9, mandelbrot/7]).

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

% extract all screen coords
screen(W, H)->
    [ {X,Y} || Y <- lists:seq(0, H - 1), X <- lists:seq(0, W - 1) ].

% convert bitmap point to complex point
complex(X, Y, MinX, MinY, FactX, FactY)->
    { MinX + X * FactX, MinY + Y * FactY }.

iterate(_, _, _, _, MaxIterations, Iterations) 
    when Iterations >= MaxIterations -> Iterations;

iterate(Zre, Zim, _, _,  _, Iterations) 
    when Zre * Zre + Zim * Zim  > 4 -> Iterations;

iterate(Zre, Zim, Cre, Cim, MaxIterations, Iterations)->
    iterate(
        Zre * Zre - Zim * Zim + Cre, 
        2 * Zre * Zim + Cim, 
        Cre, Cim,
        MaxIterations, Iterations+1
    ).

%------------------------------------------------------------------------
% Julia's set
%
% Z1 = C0, Zn+1 = Zn^2 + K
%------------------------------------------------------------------------
julia(X, Y, MinX, MinY, FactX, FactY, Kre, Kim, MaxIterations)->
    {Cre, Cim} = complex(X, Y, MinX, MinY, FactX, FactY),
    iterate(Cre, Cim, Kre, Kim, MaxIterations, 0).

%------------------------------------------------------------------------
% Mandelbrot's set
%
% Z1 = 0, Zn+1 = Zn^2 + C0
%------------------------------------------------------------------------
mandelbrot(X, Y, MinX, MinY, FactX, FactY, MaxIterations)->
    {Cre, Cim} = complex(X, Y, MinX, MinY, FactX, FactY),
    iterate(0, 0, Cre, Cim, MaxIterations, 0).

% Julia's set exploration
generate(julia, W, H, MinX, MaxX, MinY, MaxY, Kre, Kim, MaxIterations) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    FactX = (MaxX - MinX) / W,
    FactY = (MaxY - MinY) / H,
    [ julia(X, Y, MinX, MinY, FactX, FactY, Kre, Kim, MaxIterations) || 
        {X, Y} <- screen(W, H) ].

% Mandelbrot's set exploration
generate(mandelbrot, W, H, MinX, MaxX, MinY, MaxY, MaxIterations) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    FactX = (MaxX - MinX) / W,
    FactY = (MaxY - MinY) / H,
    [ mandelbrot(X, Y, MinX, MinY, FactX, FactY, MaxIterations) || 
        {X, Y} <- screen(W, H) ].

% save Julia's set in ppm format
save(julia, W, H, MinX, MaxX, MinY, MaxY, Kre, Kim, MaxIterations, FileName) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    M = [
            paletteblu:color(Iterations, MaxIterations) || 
                Iterations <- generate(
                    julia, W, H, MinX, MaxX, MinY, MaxY, Kre, Kim, MaxIterations
                )
    ],
    ppm:save(image, W, H, 255, FileName, M).

% save Mandelbrot's set in ppm format
save(mandelbrot, W, H, MinX, MaxX, MinY, MaxY, MaxIterations, FileName) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    M = [
            palettered:color(Iterations, MaxIterations) || 
                Iterations <- generate(
                    mandelbrot, W, H, MinX, MaxX, MinY, MaxY, MaxIterations
                )
    ],
    ppm:save(image, W, H, 255, FileName, M).
