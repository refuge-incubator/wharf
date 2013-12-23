
%% db record type
-record(sdb, {
        name,
        path,
        server_url,
        conn_options,
        sync_fun = <<>>,
        db=nil}).


-define(DEFAULT_SERVER_URL, << "http://localhost:5984" >>).

-define(DEFAULT_SYNC_FUN, << "
function(doc) {
    channel(doc.channels);
}" >>).


-define(SYNC_FUN_WRAP, fun(Fun) ->
            <<"function(doc, oldDoc, realUserCtx) {",
"var f=", Fun/binary, ";", "
// Proxy userCtx that allows queries but not direct access to user/roles:
var shouldValidate = (realUserCtx != null && realUserCtx.name != null);

function requireUser(names) {
    if (!shouldValidate) return;
    names = makeArray(names);
    if (!inArray(realUserCtx.name, names))
        throw({forbidden: 'wrong user'});
}

function requireRole(roles) {
    if (!shouldValidate) return;
    roles = makeArray(roles);
    if (!anyInArray(realUserCtx.roles, roles))
        throw({forbidden: 'missing role'});
}

function requireAccess(channels) {
    if (!shouldValidate) return;
    channels = makeArray(channels);
    if (!anyInArray(realUserCtx.channels, channels))
        throw({forbidden: 'missing channel access'});
}

__channels = [];

function channel(channels) {
    __channels = _channels.concat(makeArray(channels));
}

try {
    f(newDoc, oldDoc);
} catch(e) {
    if ((e.forbidden) || (e.unauthorized)) {
        return {'error': e};
    } else {
        throw(e);
    }
}

return __channels;
} ">>
    end).
