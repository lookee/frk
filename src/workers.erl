-module(workers).
-author('LucaAmore').
-export([mandelbrot/9,julia/11]).

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

% extract all screen coords (splitted in ProcessCount partitions)
screen_split(W, H, Process, ProcessCount)->
    ProcessLast = ProcessCount - 1,
    Chunk = H div ProcessCount,
    [
        {X,Y} ||
        Y <-
            if
                Process /= ProcessLast -> lists:seq(Chunk * Process, Chunk * (Process + 1));
                Process == ProcessLast -> lists:seq(Chunk * Process + 1, H - 1)
            end,
        X <- lists:seq(0, W - 1)
    ].

% it starts sliced Mandelbrot's set parallel exploration
mandelbrot(W, H, MinX, MaxX, MinY, MaxY, MaxIterations, Process, ProcessCount) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    FactX = (MaxX - MinX) / W,
    FactY = (MaxY - MinY) / H,
    [ 
        frk:mandelbrot(X, Y, MinX, MinY, FactX, FactY, MaxIterations) || 
            {X,Y} <- screen_split(W, H, Process, ProcessCount) 
    ].

% it starts sliced Julia's set parallel exploration
julia(W, H, MinX, MaxX, MinY, MaxY, Kre, Kim, MaxIterations, Process, ProcessCount) 
    when W > 0, H > 0, MaxX > MinX, MaxY > MinY, MaxIterations > 0 ->
    FactX = (MaxX - MinX) / W,
    FactY = (MaxY - MinY) / H,
    [ 
        frk:julia(X, Y, MinX, MinY, FactX, FactY, Kre, Kim, MaxIterations) || 
            {X,Y} <- screen_split(W, H, Process, ProcessCount) 
    ].
