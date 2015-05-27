%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.18.4 on {{2015,5,27},{23,50,55}}

-ifndef(protocol).
-define(protocol, true).

-define(protocol_gpb_version, "3.18.4").

-record('ProtocolUnit',
        {id,                            % = 1, string
         pos = [],                      % = 2, [float]
         move_vector = [],              % = 3, [float]
         name,                          % = 4, string (optional)
         size,                          % = 5, float (optional)
         color                          % = 6, int32 (optional)
        }).

-record('ProtocolUnitUpdate',
        {units = []                     % = 1, [{msg,'ProtocolUnit'}]
        }).

-record('ProtocolUnitAdd',
        {units = []                     % = 1, [{msg,'ProtocolUnit'}]
        }).

-record('ProtocolUnitRemove',
        {ids = []                       % = 1, [string]
        }).

-endif.
