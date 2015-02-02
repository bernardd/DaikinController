$(document).ready(function() {
		$(".toggle").addClass("btn btn-danger btn-block");
		setAlert("alert-warning", "Attempting to connect");

		var ws = new WebSocket("ws://pico:8080/socket");
		ws.onopen = function() {
			$("#alert").slideUp();
		}

		ws.onerror = function() {
			setAlert("alert-danger", "WebSocket error");
		}

		ws.onclose = function() {
			setAlert("alert-danger", "WebSocket was closed");
		}

		ws.onmessage = handleUpdate;
});

function setAlert(type, text) {
	$("#alert").removeClass("alert-success alert-info alert-warning alert-danger").addClass(type).html(text).slideDown();
}

function setToggle(id, val) {
	var button = $("#"+id);
	button.removeClass("btn-danger btn-success");
	if (val == 0)
		button.addClass("btn-danger");
	else
		button.addClass("btn-success");
}

function handleUpdate(message) {
	var j = JSON.parse(message.data);
	$.each( j, function( key, value ) {
		switch(key) {
			case "time":
			case "turn_on_time":
			case "turn_off_time":
				$("#"+key).val(value);
				break;
			case "fan":
			case "mode":
				var select = $("#"+key);
				select.find("option").removeAttr("selected");
				select.find("#"+key+"-"+value).attr("selected", "true");
				break;
			default:
				setToggle(key, value);
				break;
		}
	});
}
