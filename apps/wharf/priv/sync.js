function makeArray(maybeArray) {
    if (Array.isArray(maybeArray)) {
        return maybeArray;
    } else {
        return [maybeArray];
    }
}

function inArray(string, array) {
    return array.indexOf(string) != -1;
}

function anyInArray(any, array) {
    for (var i = 0; i < any.length; ++i) {
        if (inArray(any[i], array))
            return true;
    }
    return false;
}
