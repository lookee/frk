-module(ppm).
-author('LucaAmore').
-export([save/6]).

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

save(image,W,H,M,FileName,Data) ->
    {ok, FileId} = file:open(FileName, [write]),
    io:fwrite(FileId, "P3~n",[]),
    io:fwrite(FileId, "~p ~p~n",[W,H]),
    io:fwrite(FileId, "~p~n",[M]),
    write_data(FileId,Data,1),
    file:close(FileId),
    ok.

write_data(_,[],_)->
    ok;

write_data(FileId,[H|Data],Cnt)->
    {R, G, B } = H,
    case Cnt rem 4 of
        0 -> io:fwrite(FileId, "~p ~p ~p~n", [R, G, B]);
        _ -> io:fwrite(FileId, "~p ~p ~p ", [R, G, B])
    end,
    write_data(FileId,Data,Cnt+1).
