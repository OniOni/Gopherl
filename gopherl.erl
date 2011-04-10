-module(gopherl).
-author("Mathieu Sabourin").

-export([listen/1, start/1, list_menu/0, parse_menu_mine/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, once}, {reuseaddr, true}]).

central({init, Menu}) ->
    Dict = dict:from_list(parse_menu_mine(Menu, init)),
    central(Dict);

central(Dict) ->
    receive
	{get, Key, Pid} ->
	    Pid ! dict:find(Key, Dict);
	{getDir, Dir, Pid} ->
	    Pid ! dict:to_list(
		    dict:filter(fun(_, Value) ->
					case Value of
					    {Dir2, _} ->
						if
						    Dir == Dir2 ->
							true;
						    true ->
							false
						end;
					    _Other ->
						false
					end
				end,
			       Dict)
		   );
	{all, Pid} ->
	    Pid ! dict:to_list(Dict);
	{menu, Pid} ->
	    Pid ! dict:to_list(
		    dict:filter(fun(_, Value) ->
					case Value of
					    {"files", _} ->
						true;
					    _Other ->
						false
					end
				end,
			       Dict)
		   )
    end,
    central(Dict).

start(Port) ->
    register(central, spawn(fun() -> central({init, "files"}) end)),
    listen(Port).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).


accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
	{ok, Socket} ->
	    inet:setopts(Socket, ?TCP_OPTIONS),
	    io:format("Accept~n"),
	    gen_tcp:controlling_process(Socket, spawn(fun() ->  loop(Socket) end)),
	    accept(LSocket);
	Other ->
	    io:format("Got [~w]~n", [Other]),
	    ok
    end.

file_t_to_str(T) ->
    case T of
	{Name, {Dir, directory}} ->
	    Path = (Dir ++ "/") ++ Name,
	    (((("1" ++ Name) ++ "\t") ++ Path) ++ "\tlocalhost\t7000\r\n");
	{Name, {Dir, regular}} ->
	    Path = (Dir ++ "/") ++ Name,
	    (((("0" ++ Name) ++ "\t") ++ Path) ++ "\tlocalhost\t7000\r\n")
    end.

process_file_data(Data) ->
    case Data of
	[H | []] ->
	    file_t_to_str(H) ++ ".\r\n";
	[H | T] ->	    
	    file_t_to_str(H) ++ process_file_data(T)
    end.	
    
parse_menu(Menu) ->
    lists:map(fun(File) -> Dir = Menu ++ "/", 
			   case file:read_file_info(Dir ++ File) of
			       {ok, Data} ->
				   {File, {Menu, element(3, Data)}};
			       Other -> 
				   {error, Other}
			   end
	      end, 
	      element(2, file:list_dir(Menu))).

parse_file(Menu, File) ->
    Dir = Menu ++ "/", 
    case file:read_file_info(Dir ++ File) of
	{ok, Data} ->
	    {File, {Menu,element(3, Data)}};
	Other -> 
	    {error, Other}    
    end.

parse_menu_mine(Menu, init) ->
    List = element(2, file:list_dir(Menu)),
    parse_menu_mine(Menu, List);
parse_menu_mine(Menu, List) ->
    case List of
	[H | []] ->
	    Tmp = parse_file(Menu, H),
	    case Tmp of 
		{Name, {_, directory}} ->
		    [Tmp] ++ parse_menu_mine((Menu ++ "/") ++ Name, init);
		{_, {_, regular}} ->
		    [Tmp]
	    end;
	[H | T] ->
	    Tmp = parse_file(Menu, H),
	    case Tmp of 
		{Name, {_, directory}} ->
		    ([Tmp] ++ parse_menu_mine((Menu ++ "/") ++ Name, init))
			++ parse_menu_mine(Menu, T);
		{_, {_, regular}} ->
		    [Tmp] ++ parse_menu_mine(Menu, T)
	    end	
    end.
		
list_menu() ->
    central ! {menu, self()},
    receive 
	List ->
	    process_file_data(List)
    end.

read_all(init, File) ->    
    {ok, Io} = file:open(File, read),
    read_all(Io, []);

read_all(Io, Data) ->
    case file:read_line(Io) of
	{ok, More} ->
	    read_all(Io, Data ++ More);
	eof ->
	    file:close(Io),
	    Data;
	Other ->
	    io:format("got ~p~n", [Other]),
	    Data
    end.

clean(Str) ->
    (((Str -- " ") -- "\n") -- "\r").

answer(Request) ->
    Tmp = lists:last(string:tokens(clean(Request), "/")),    
    central ! {get, clean(Tmp), self()},
    receive 
	{ok, {_, directory}} ->
	    central ! {getDir, clean(Request), self()},
	    receive 
		List ->
		    process_file_data(List)
	    end;
	{ok, {_, regular}} ->
	    read_all(init, clean(Request)) ++ "\r\n";
	error ->	    
	    "File Not Found\r\n"
    end.

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
	    case bitstring_to_list(Data) of
		[13, 10] -> %Receive CR LF
		    io:format("Got CR LF~n"),
		    gen_tcp:send(Socket, 
				 list_menu());
		File ->
		    io:format("Got ~p~n", [clean(File)]),
		    gen_tcp:send(Socket, 
				 answer(File))
	    end		
    end.

