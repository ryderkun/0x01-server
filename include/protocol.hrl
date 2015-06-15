%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.18.4 on {{2015,6,15},{16,21,20}}

-ifndef(protocol).
-define(protocol, true).

-define(protocol_gpb_version, "3.18.4").

-record('ProtocolVector2',
        {x,                             % = 1, float
         y                              % = 2, float
        }).

-record('ProtocolUnitCreate',
        {name,                          % = 1, string
         pos                            % = 2, {msg,'ProtocolVector2'}
        }).

-record('ProtocolUnitMove',
        {target                         % = 1, {msg,'ProtocolVector2'}
        }).

-record('ProtocolUnitEject',
        {
        }).

-record('ProtocolDot',
        {id,                            % = 1, string
         pos,                           % = 2, {msg,'ProtocolVector2'}
         color                          % = 3, int32
        }).

-record('ProtocolUnit',
        {id,                            % = 1, string
         pos,                           % = 2, {msg,'ProtocolVector2'}
         size,                          % = 3, float (optional)
         name,                          % = 4, string (optional)
         color                          % = 5, int32 (optional)
        }).

-record('ProtocolSceneInit',
        {unit_adds = [],                % = 1, [{msg,'ProtocolUnit'}]
         dot_adds = []                  % = 2, [{msg,'ProtocolDot'}]
        }).

-record('ProtocolUnitAdd',
        {is_own,                        % = 1, bool
         units = []                     % = 2, [{msg,'ProtocolUnit'}]
        }).

-record('ProtocolSceneSync',
        {update_at,                     % = 1, int64
         unit_updates = [],             % = 2, [{msg,'ProtocolUnit'}]
         unit_removes = [],             % = 3, [string]
         dot_adds = [],                 % = 4, [{msg,'ProtocolDot'}]
         dot_removes = []               % = 5, [string]
        }).

-record('ProtocolConfig',
        {sync_interval,                 % = 1, float
         map_minx,                      % = 2, int32
         map_miny,                      % = 3, int32
         map_maxx,                      % = 4, int32
         map_maxy                       % = 5, int32
        }).

-record('ProtocolUnitSplit',
        {
        }).

-record('ProtocolTimeSync',
        {client,                        % = 1, int64
         server                         % = 2, int64
        }).

-endif.
