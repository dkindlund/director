-module(director_helper).
-compile(export_all).

%% @spec get_base_dir(Module) -> string()
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

%% @spec get_base_dir() -> string()
%% @doc Return the application directory for this application. Equivalent to
%%      get_base_dir(?MODULE).
get_base_dir() ->
    get_base_dir(?MODULE).

%% @spec local_path([string()], Module) -> string()
%% @doc Return an application-relative directory from Module's application.
local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

%% @spec local_path(Components) -> string()
%% @doc Return an application-relative directory for this application.
%%      Equivalent to local_path(Components, ?MODULE).
local_path(Components) ->
    local_path(Components, ?MODULE).


%build_rake_command(RailsDir,Command) ->
    %AppDir = local_path(["applications",RailsDir,"Rakefile"]),
    %lists:flatten(io_lib:format("rake -f~s ~s",[AppDir,Command])).
 
build_rake_command(Command) ->   
    lists:flatten(io_lib:format("rake ~s",[Command])).

pidfile_location(RailsDir,PidFile) ->
    local_path(["applications",RailsDir,PidFile]).

application_root(RailsDir) ->    
    local_path(["applications",RailsDir]).

%% Returns a list of the names of Rails apps under the applications directory
list_rails_apps() ->
    All = filelib:wildcard(local_path(["applications","*"])),
    Names = lists:map(fun(N) ->
			     filename:basename(N)
		     end,
		     All),
    lists:sort(Names).

