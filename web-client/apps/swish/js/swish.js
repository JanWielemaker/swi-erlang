var env = {};


env.editor = ace.edit("editor");
env.editor.setTheme("ace/theme/brain");
env.editor.getSession().setMode("ace/mode/prolog");
env.editor.setHighlightActiveLine(false);
env.editor.setDisplayIndentGuides(false);
env.editor.renderer.setShowPrintMargin(false);
env.editor.session.setFoldStyle("manual");
env.editor.renderer.setVScrollBarAlwaysVisible(true);


// Getting and setting program and goal

function getProgram() {
    return env.editor.getValue()
}

function setProgram(src) {
	env.editor.setValue(src, -1);
}


function theme() {
    return env.editor.renderer.theme.cssClass;
}


// Printing

function print_editor_content() {
	var iframe = document.createElement("iframe");
	iframe.style.display = "none"
	document.body.appendChild(iframe)
	var windw = iframe.contentWindow;
	windw.document.open();
    windw.document.write('</head><body><pre>');
    windw.document.write(getProgram());
    windw.document.write('</pre></body></html>');
    windw.print();
    windw.document.close();
    document.body.removeChild(iframe);
}


// GUI preferences

function setTheme(theme) {
	env.editor.setTheme("ace/theme/" + theme);
	$("#theme-menu option:selected").prop("selected", false);
	$("#theme-menu").find("option[value='" + theme +"']").prop("selected", true);
}

function setFontFamily(family) {
	$('#editor').css('fontFamily', family);
	$("#font-family-menu option:selected").prop("selected", false);
	$("#font-family-menu").find("option[value='" + family +"']").prop("selected", true);
}

function setFontSize(size) {
	$('#editor').css('fontSize', size + 'px');
	$("#font-size-menu option:selected").prop("selected", false);
	$("#font-size-menu").find("option[value=" + size +"]").prop("selected", true);
}

function setTabSize(n) {
	env.editor.getSession().setTabSize(n);
	$("#tab-size-menu option:selected").prop("selected", false);
	$("#tab-size-menu").find("option[value=" + n +"]").prop("selected", true);
}

function setUseSoftTabs(bool) {
	env.editor.getSession().setUseSoftTabs(bool);
	$("#tab-soft-checkbox").prop('checked', bool);
}

function setLineWrap(bool) {
	env.editor.getSession().setUseWrapMode(bool);
	$("#line-wrap-checkbox").prop('checked', bool);
}

function setLineHighlight(bool) {
	env.editor.setHighlightActiveLine(bool);
	$("#line-highlight-checkbox").prop('checked', bool);
}

function setShowGutter(bool) {
	env.editor.renderer.setShowGutter(bool);
	$("#line-numbering-checkbox").prop('checked', bool);
}


// Handling programs

function maybeLoadSrc() {
    var file = window.location.hash.slice(1);
    if (file) {
        loadSrc("/storage/"+ encodeURIComponent(file));
    }
}

function loadSrc(url) {
    $.get(url)
    .done(function(program) {
		setProgram(program);
		env.dirty = true;
	})
	.fail(function() {
		alert("Error loading program.")
	})
}

function saveProgram() {
    var program = encodeURIComponent(getProgram());
    if (program) {
        $.post("/storage/store", "program=" + program, function(response) {
            var url = response.url;
            var file = response.file;
            window.location.hash = file;
            $("#url").val(url + "/apps/swish/index.html#" + file);
            env.dirty = false;
        });
    }
}

function updateProgram() {
	var file = window.location.hash.slice(1);
    var program = encodeURIComponent(getProgram());
    if (program) {
         $.post("/storage/update", "file=" + file + "&program=" + program, function() {
            env.dirty = false;
        });
    }
}



// Event handlers: Editor

env.editor.getSession().on('change', function() {
	if (!env.dirty) {
	    env.dirty = true;
	}
});


// Event handlers: Menus

$("#file-menu").on("click", "a#new", function(evt) {
	evt.preventDefault();
	window.location.hash = "";
	setProgram("% Your program goes here\n\n\n\n/** Examples\n\n\n*/\n");
	env.dirty = false;
});

$("#file-menu").on("click", "a#save", function(evt) {
	evt.preventDefault();
	if (window.location.hash == "") {
	    saveProgram();
	} else {
	    updateProgram();
	}
});

$("#file-menu").on("click", "a#share", function(evt) {
	evt.preventDefault();
	if (window.location.hash == "") {
	    saveProgram();
	} else {
	    updateProgram();
	}
    $('#share-dialog').modal();
});

$("#file-menu").on("click", "a#collaborate", function(evt) {
	evt.preventDefault();
	TogetherJS(this);
});

$("#file-menu").on("click", "a#prefs", function(evt) {
	evt.preventDefault();
	$("#preferences").modal({backdrop:false});
});

$("#file-menu").on("click", "a#print", function(evt) {
	evt.preventDefault();
	print_editor_content();
});

$("#edit-menu").on("click", "a#undo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.undo.exec(env.editor)
});

$("#edit-menu").on("click", "a#redo", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.redo.exec(env.editor)
});

$("#edit-menu").on("click", "a#indent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.indent.exec(env.editor)
});

$("#edit-menu").on("click", "a#outdent", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.outdent.exec(env.editor)
});

$("#edit-menu").on("click", "a#comment", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.toggleBlockComment.exec(env.editor)
});

$("#edit-menu").on("click", "a#find", function(evt) {
	evt.preventDefault();
	env.editor.commands.commands.replace.exec(env.editor, "left")
});

$("#shell-menu").on("click", "a#clear", function(evt) {
	evt.preventDefault();
    gterm.clear();
    setTimeout(gterm.enable, 0);
});

$("#shell-menu").on("click", "a#json-trace", function(evt) {
	evt.preventDefault();
	if (trace) {
	    trace = false;
	} else {
	    trace = true;
	};
    setTimeout(gterm.enable, 0);
});

$("#example-menu").on("click", "a", function(evt) {
	evt.preventDefault();
    if (evt.target.id == "tut") {
        $("#editor").css("display","none");
        $("#tutorial").css("display","block");
    } else {
        $("#editor").css("display","block");
        $("#tutorial").css("display","none");
	    window.location.hash = "";
	    loadSrc(evt.target.href);
    }
});

// Event handlers: Preferences

$("#theme-menu").on("change", function() {
	var value = $("#theme-menu option:selected").val();
	setTheme(value);
	if (localStorage) {
		localStorage['swish-theme'] = value;
	}
});

$("#font-family-menu").on("change", function() {
	var value = $("#font-family-menu option:selected").val();
	setFontFamily(value);
	if (localStorage) {
		localStorage['swish-font-family'] = value;
	}
});

$("#font-size-menu").on("change", function() {
	var value = $("#font-size-menu option:selected").val();
	setFontSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-font-size'] = value;
	}
});

$("#tab-size-menu").on("change", function() {
	var value = $("#tab-size-menu option:selected").val();
	setTabSize(parseInt(value, 10));
	if (localStorage) {
		localStorage['swish-tab-size'] = value;
	}
});

$("#tab-soft-checkbox").on("change", function() {
	var value = $("#tab-soft-checkbox").prop('checked');
	setUseSoftTabs(value);
	if (localStorage) {
		localStorage['swish-tab-soft'] = value;
	}
});

$("#line-wrap-checkbox").on("change", function() {
	var value = $("#line-wrap-checkbox").prop('checked');
	setLineWrap(value);
	if (localStorage) {
		localStorage['swish-line-wrap'] = value;
	}
});

$("#line-highlight-checkbox").on("change", function() {
	var value = $("#line-highlight-checkbox").prop('checked');
	setLineHighlight(value);
	if (localStorage) {
		localStorage['swish-line-highlight'] = value;
	}
});

$("#line-numbering-checkbox").on("change", function() {
	var value = $("#line-numbering-checkbox").prop('checked');
	setShowGutter(value);
	if (localStorage) {
		localStorage['swish-line-numbering'] = value;
	}
});

$("#slider").on("input", function() {
    var val = this.value;
    $("#editor").css("width", val+"%");
    $("#tutorial").css("width", val+"%");
    $("#shell").css("width", (100-val)+"%");
});


/*
// Event handlers: Console

$("#examples-btn").on("click", function() {
	if (env.dirty) {
		populateExampleMenu();
	}
});


$("#history-btn").on("click", populateHistoryMenu);

$("#clear-btn-query").on("click", function() {
	setGoal("?- ")
});

$("#first-btn").on("click", first);
$("#more-btn").on("click", more);
$("#stop-btn").on("click", stop);
$("#abort-btn").on("click", abort);
$("#clear-btn").on("click", clear);

$("#reader").on("keyup", function(evt) {
	if (evt.keyCode == 13) {
		read();
	}
});

$("#reader").on("blur", function(evt) {
	evt.target.focus();
	return false;
});
*/

function parseBoolean(value) {
	return value == "true" ? true : false;
}

// Initialisation

$(document).ready(function() {
	if (localStorage && localStorage.length > 0) {
		setTheme(localStorage['swish-theme']);
		setFontFamily(localStorage['swish-font-family']);
		setFontSize(localStorage['swish-font-size']);
		setTabSize(parseInt(localStorage['swish-tab-size'], 10));
		setLineWrap(parseBoolean(localStorage['swish-line-wrap']));
		setLineHighlight(parseBoolean(localStorage['swish-line-highlight']));
		setShowGutter(parseBoolean(localStorage['swish-line-numbering']));
		setUseSoftTabs(parseBoolean(localStorage['swish-tab-soft']));
	}
    maybeLoadSrc();
});

