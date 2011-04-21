-module(parallel).
-author('LucaAmore').
-export([mandelbrot/10,julia/12]).

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

mandelbrot(W, H, MinX, MaxX, MinY, MaxY, MaxIterations, FileName, N, Palette) ->
    Args = [W, H, MinX, MaxX, MinY, MaxY, MaxIterations],
    M = [
            apply(Palette,color,[Iterations, MaxIterations]) || 
                Iterations <- run(mandelbrot, Args, N)
    ],
    ppm:save(image,W,H,255,FileName,M).

julia(W, H, MinX, MaxX, MinY, MaxY, KIm, KRe, MaxIterations, FileName, N, Palette) ->
    Args = [W, H, MinX, MaxX, MinY, MaxY, KIm, KRe, MaxIterations],
    M = [
            apply(Palette,color,[Iterations, MaxIterations]) || 
                Iterations <- run(julia, Args, N)
    ],
    ppm:save(image,W,H,255,FileName,M).

run(Func, Args, N) ->
    MyPid = self(),
    Pids = lists:map(
        fun(_X) -> spawn(fun() -> worker(MyPid) end) end,
        lists:seq(1, N)
    ),
    send_work(Pids, MyPid, Func, Args, N),
    receive_work(Pids,[]).

% send request to workers
send_work([], _, _, _, _)->
    true;

send_work(PidList, MyPid, Func, Args, N) ->
    [Pid | Pids] = PidList,
    WorkerN = N - length(PidList),
    Pid ! {MyPid, Func, Args, WorkerN, N},
    send_work(Pids, MyPid, Func, Args, N).

% receive and aggregate reply from workers
receive_work([],Out)-> Out;

receive_work(PidList,Out) ->
    [Pid | Pids] = PidList,
    receive
        {Pid, Reply} ->
            receive_work(Pids, Out ++ Reply)
    end.

% workers 
% receive requests from parent, process them, reply to parent, terminate
worker(Parent) ->
    receive
        {Parent, Func, Args, WorkerN, WorkerCount } ->
            Reply = apply(workers, Func, Args ++ [WorkerN, WorkerCount]),
            Parent ! {self(), Reply}
    end.
