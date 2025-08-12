%%% @doc Comprehensive tests for eorm_registry to achieve 100% coverage
%%% @end
-module(eorm_registry_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").
-include("eorm.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

setup() ->
    %% Start the registry if not already started
    case whereis(eorm_registry) of
        undefined ->
            {ok, Pid} = eorm_registry:start_link(),
            erlang:put(registry_pid, Pid);
        Pid ->
            erlang:put(registry_pid, Pid)
    end,
    
    %% Start meck for mocking
    application:ensure_all_started(meck),
    ok.

teardown(_) ->
    %% Stop the registry if we started it
    case erlang:get(registry_pid) of
        undefined -> ok;
        Pid ->
            case is_process_alive(Pid) of
                true -> 
                    gen_server:stop(Pid, normal, 5000);
                false -> 
                    ok
            end
    end,
    
    %% Unload mocks
    catch meck:unload(),
    ok.

%%====================================================================
%% Test Suites
%%====================================================================

registry_suite_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         fun test_register_valid_model/0,
         fun test_register_invalid_model/0,
         fun test_get_registered_model/0,
         fun test_get_unregistered_model/0,
         fun test_list_models/0,
         fun test_parse_model_with_schema/0,
         fun test_parse_model_with_definition/0,
         fun test_parse_model_without_schema/0,
         fun test_parse_model_error_handling/0,
         fun test_parse_fields_with_timestamps/0,
         fun test_parse_field_formats/0,
         fun test_infer_field_types/0,
         fun test_parse_associations/0,
         fun test_infer_foreign_key/0,
         fun test_handle_unknown_call/0,
         fun test_handle_cast/0,
         fun test_handle_info/0,
         fun test_code_change/0
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_register_valid_model() ->
    %% Mock a valid model
    meck:new(valid_test_model, [non_strict]),
    meck:expect(valid_test_model, schema, fun() ->
        #{
            table => test_table,
            fields => [
                {id},
                {name, string}
            ]
        }
    end),
    
    %% Register the model
    Result = eorm_registry:register_model(valid_test_model),
    ?assertEqual(ok, Result),
    
    %% Verify it was registered
    {ok, Model} = eorm_registry:get_model(valid_test_model),
    ?assertMatch(#eorm_model{module = valid_test_model}, Model),
    
    meck:unload(valid_test_model).

test_register_invalid_model() ->
    %% Mock an invalid model (no schema or definition)
    meck:new(invalid_test_model, [non_strict]),
    
    %% Try to register the invalid model
    Result = eorm_registry:register_model(invalid_test_model),
    ?assertMatch({error, {invalid_model, invalid_test_model}}, Result),
    
    meck:unload(invalid_test_model).

test_get_registered_model() ->
    %% Mock a model and register it
    meck:new(get_test_model, [non_strict]),
    meck:expect(get_test_model, schema, fun() ->
        #{fields => [{id}, {name, string}]}
    end),
    
    eorm_registry:register_model(get_test_model),
    
    %% Get the registered model
    Result = eorm_registry:get_model(get_test_model),
    ?assertMatch({ok, #eorm_model{module = get_test_model}}, Result),
    
    meck:unload(get_test_model).

test_get_unregistered_model() ->
    %% Try to get a model that wasn't registered
    Result = eorm_registry:get_model(unregistered_model),
    ?assertEqual({error, not_found}, Result).

test_list_models() ->
    %% Mock and register multiple models
    meck:new(list_model1, [non_strict]),
    meck:expect(list_model1, schema, fun() -> #{fields => [{id}]} end),
    
    meck:new(list_model2, [non_strict]),
    meck:expect(list_model2, schema, fun() -> #{fields => [{id}]} end),
    
    eorm_registry:register_model(list_model1),
    eorm_registry:register_model(list_model2),
    
    %% List all models
    Models = eorm_registry:list_models(),
    ?assert(lists:member(list_model1, Models)),
    ?assert(lists:member(list_model2, Models)),
    
    meck:unload(list_model1),
    meck:unload(list_model2).

test_parse_model_with_schema() ->
    %% Mock a model with schema function
    meck:new(schema_model, [non_strict]),
    meck:expect(schema_model, schema, fun() ->
        #{
            table => my_table,
            fields => [{id}, {name, string}],
            indexes => [#{name => idx_name, fields => [name]}],
            associations => [],
            constraints => [],
            hooks => [],
            options => []
        }
    end),
    
    Result = eorm_registry:register_model(schema_model),
    ?assertEqual(ok, Result),
    
    meck:unload(schema_model).

test_parse_model_with_definition() ->
    %% Mock a model with definition function instead of schema
    meck:new(definition_model, [non_strict]),
    meck:expect(definition_model, definition, fun() ->
        #{
            table => def_table,
            fields => [{id}, {email, string}]
        }
    end),
    
    Result = eorm_registry:register_model(definition_model),
    ?assertEqual(ok, Result),
    
    meck:unload(definition_model).

test_parse_model_without_schema() ->
    %% Mock a model without schema or definition
    meck:new(no_schema_model, [non_strict]),
    
    Result = eorm_registry:register_model(no_schema_model),
    ?assertMatch({error, {invalid_model, no_schema_model}}, Result),
    
    meck:unload(no_schema_model).

test_parse_model_error_handling() ->
    %% Mock a model that throws an error in schema function
    meck:new(error_model, [non_strict]),
    meck:expect(error_model, schema, fun() ->
        error(schema_error)
    end),
    
    Result = eorm_registry:register_model(error_model),
    ?assertMatch({error, {parse_error, _, _}}, Result),
    
    meck:unload(error_model).

test_parse_fields_with_timestamps() ->
    %% Mock a model with timestamps special field
    meck:new(timestamp_model, [non_strict]),
    meck:expect(timestamp_model, schema, fun() ->
        #{
            fields => [
                {id},
                {name, string},
                timestamps
            ]
        }
    end),
    
    Result = eorm_registry:register_model(timestamp_model),
    ?assertEqual(ok, Result),
    
    %% Verify the timestamps were expanded
    {ok, Model} = eorm_registry:get_model(timestamp_model),
    Fields = Model#eorm_model.fields,
    
    %% Check that created_at and updated_at were added
    FieldNames = [F#eorm_field.name || F <- Fields],
    ?assert(lists:member(created_at, FieldNames)),
    ?assert(lists:member(updated_at, FieldNames)),
    
    meck:unload(timestamp_model).

test_parse_field_formats() ->
    %% Mock a model with various field formats
    meck:new(field_format_model, [non_strict]),
    meck:expect(field_format_model, schema, fun() ->
        #{
            fields => [
                {id},                                          % Auto-inferred
                {name, string},                                % With type
                {age, integer, [not_null]},                    % With type and options
                #{name => email, type => string, opts => [unique]},  % Map format
                {user_id},                                     % Auto-infer foreign key
                {created_at},                                  % Auto-infer timestamp
                {is_active},                                   % Auto-infer boolean
                {custom_field}                                 % Default to string
            ]
        }
    end),
    
    Result = eorm_registry:register_model(field_format_model),
    ?assertEqual(ok, Result),
    
    %% Verify field parsing
    {ok, Model} = eorm_registry:get_model(field_format_model),
    Fields = Model#eorm_model.fields,
    
    %% Find specific fields and verify their properties
    IdField = lists:keyfind(id, #eorm_field.name, Fields),
    ?assertEqual(integer, IdField#eorm_field.type),
    ?assertEqual(true, IdField#eorm_field.primary_key),
    
    NameField = lists:keyfind(name, #eorm_field.name, Fields),
    ?assertEqual(string, NameField#eorm_field.type),
    ?assertEqual(true, NameField#eorm_field.nullable),
    
    AgeField = lists:keyfind(age, #eorm_field.name, Fields),
    ?assertEqual(integer, AgeField#eorm_field.type),
    ?assertEqual(false, AgeField#eorm_field.nullable),
    
    EmailField = lists:keyfind(email, #eorm_field.name, Fields),
    ?assertEqual(string, EmailField#eorm_field.type),
    ?assert(lists:member(unique, EmailField#eorm_field.options)),
    
    UserIdField = lists:keyfind(user_id, #eorm_field.name, Fields),
    ?assertEqual(integer, UserIdField#eorm_field.type),
    
    CreatedAtField = lists:keyfind(created_at, #eorm_field.name, Fields),
    ?assertEqual(timestamp, CreatedAtField#eorm_field.type),
    
    IsActiveField = lists:keyfind(is_active, #eorm_field.name, Fields),
    ?assertEqual(boolean, IsActiveField#eorm_field.type),
    
    CustomField = lists:keyfind(custom_field, #eorm_field.name, Fields),
    ?assertEqual(string, CustomField#eorm_field.type),
    
    meck:unload(field_format_model).

test_infer_field_types() ->
    %% Test field type inference directly
    meck:new(infer_model, [non_strict]),
    meck:expect(infer_model, schema, fun() ->
        #{
            fields => [
                {id},           % Should infer integer with primary_key
                {user_id},      % Should infer integer (foreign key)
                {post_id},      % Should infer integer (foreign key)
                {created_at},   % Should infer timestamp
                {updated_at},   % Should infer timestamp
                {deleted_at},   % Should infer timestamp
                {is_active},    % Should infer boolean
                {is_published}, % Should infer boolean
                {is_deleted},   % Should infer boolean
                {title},        % Should infer string
                {description}   % Should infer string
            ]
        }
    end),
    
    Result = eorm_registry:register_model(infer_model),
    ?assertEqual(ok, Result),
    
    {ok, Model} = eorm_registry:get_model(infer_model),
    Fields = Model#eorm_model.fields,
    
    %% Verify all inferences
    ?assertEqual(integer, (lists:keyfind(id, #eorm_field.name, Fields))#eorm_field.type),
    ?assertEqual(integer, (lists:keyfind(user_id, #eorm_field.name, Fields))#eorm_field.type),
    ?assertEqual(timestamp, (lists:keyfind(created_at, #eorm_field.name, Fields))#eorm_field.type),
    ?assertEqual(boolean, (lists:keyfind(is_active, #eorm_field.name, Fields))#eorm_field.type),
    ?assertEqual(string, (lists:keyfind(title, #eorm_field.name, Fields))#eorm_field.type),
    
    meck:unload(infer_model).

test_parse_associations() ->
    %% Mock a model with associations
    meck:new(assoc_model, [non_strict]),
    meck:expect(assoc_model, schema, fun() ->
        #{
            fields => [{id}],
            associations => [
                {belongs_to, user, users},
                {has_many, posts, post},
                {has_one, profile, user_profile, #{foreign_key => profile_id}},
                {many_to_many, tags, tag, #{
                    join_table => posts_tags,
                    foreign_key => post_id,
                    association_foreign_key => tag_id
                }}
            ]
        }
    end),
    
    Result = eorm_registry:register_model(assoc_model),
    ?assertEqual(ok, Result),
    
    {ok, Model} = eorm_registry:get_model(assoc_model),
    Associations = Model#eorm_model.associations,
    
    %% Verify associations were parsed correctly
    ?assertEqual(4, length(Associations)),
    
    %% Check belongs_to association
    BelongsTo = lists:keyfind(user, #eorm_association.name, Associations),
    ?assertEqual(belongs_to, BelongsTo#eorm_association.type),
    ?assertEqual(users, BelongsTo#eorm_association.target),
    ?assertEqual(users_id, BelongsTo#eorm_association.foreign_key),
    
    %% Check has_many association
    HasMany = lists:keyfind(posts, #eorm_association.name, Associations),
    ?assertEqual(has_many, HasMany#eorm_association.type),
    ?assertEqual(post, HasMany#eorm_association.target),
    ?assertEqual(posts_id, HasMany#eorm_association.foreign_key),
    
    %% Check has_one with custom foreign_key
    HasOne = lists:keyfind(profile, #eorm_association.name, Associations),
    ?assertEqual(has_one, HasOne#eorm_association.type),
    ?assertEqual(user_profile, HasOne#eorm_association.target),
    ?assertEqual(profile_id, HasOne#eorm_association.foreign_key),
    
    %% Check many_to_many association
    ManyToMany = lists:keyfind(tags, #eorm_association.name, Associations),
    ?assertEqual(many_to_many, ManyToMany#eorm_association.type),
    ?assertEqual(tag, ManyToMany#eorm_association.target),
    ?assertEqual(posts_tags, ManyToMany#eorm_association.join_table),
    
    meck:unload(assoc_model).

test_infer_foreign_key() ->
    %% Test foreign key inference for different association types
    meck:new(fk_model, [non_strict]),
    meck:expect(fk_model, schema, fun() ->
        #{
            fields => [{id}],
            associations => [
                {belongs_to, author, user},      % Should infer user_id
                {has_many, comments, comment},   % Should infer comments_id
                {has_one, metadata, post_meta}   % Should infer metadata_id
            ]
        }
    end),
    
    Result = eorm_registry:register_model(fk_model),
    ?assertEqual(ok, Result),
    
    {ok, Model} = eorm_registry:get_model(fk_model),
    Associations = Model#eorm_model.associations,
    
    %% Verify foreign key inference
    Author = lists:keyfind(author, #eorm_association.name, Associations),
    ?assertEqual(user_id, Author#eorm_association.foreign_key),
    
    Comments = lists:keyfind(comments, #eorm_association.name, Associations),
    ?assertEqual(comments_id, Comments#eorm_association.foreign_key),
    
    Metadata = lists:keyfind(metadata, #eorm_association.name, Associations),
    ?assertEqual(metadata_id, Metadata#eorm_association.foreign_key),
    
    meck:unload(fk_model).

test_handle_unknown_call() ->
    %% Test unknown call handling
    %% Use the existing registry process
    Pid = whereis(eorm_registry),
    
    %% Send an unknown request
    Result = gen_server:call(Pid, unknown_request),
    ?assertEqual({error, unknown_request}, Result).

test_handle_cast() ->
    %% Test handle_cast (should just return noreply)
    %% Use the existing registry process
    Pid = whereis(eorm_registry),
    
    %% Send a cast message
    gen_server:cast(Pid, some_message),
    
    %% Verify the process is still alive
    ?assert(is_process_alive(Pid)).

test_handle_info() ->
    %% Test handle_info (should just return noreply)
    %% Use the existing registry process
    Pid = whereis(eorm_registry),
    
    %% Send an info message
    Pid ! some_info,
    
    %% Give it time to process
    timer:sleep(10),
    
    %% Verify the process is still alive
    ?assert(is_process_alive(Pid)).

test_code_change() ->
    %% Test code_change callback
    %% Use the existing registry process
    Pid = whereis(eorm_registry),
    
    %% Simulate code change
    sys:suspend(Pid),
    sys:change_code(Pid, eorm_registry, old_vsn, []),
    sys:resume(Pid),
    
    %% Verify the process is still working
    Result = eorm_registry:list_models(),
    ?assert(is_list(Result)).