"use strict";
//var ws = new WebSocket("ws://127.0.0.1:8080/indsearchalone2/echo");
var ws = new WebSocket("ws://141.14.159.165:8082/indsearchalone2/echo");
var sendPosition,
    visibilityMap = {},
    map = {},
    state = {},
    lastClicked,
    position,
    tutstate = 0,
    clickable = false;

map.structure =
    makeRandomArray(100);
map.edgeSize = 10;

visibilityMap.structure =
    new Array(100).fill(0);
visibilityMap.edgeSize = 10;


var $sidebar = $('#sidebar'),
    $leftButton = $('#leftButton'),
    $textfield = $('#textfield'),
    $game = $('#game'),
    $trainingButton = $('#trainingButton'),
    $sidebarTable = $('#sidebarTable'),
    $stepTotal = $('#stepTotal'),
    $koop = $('#koop'),
    $copyAllowed = $('#copyAllowed'),
    $copyAllowedLine = $('#copyAllowedLine'),
    $step = $('#step'),
    $level = $('#level'),
    $currentPoint = $('#currentPoint'),
    $totalPoint = $('#totalPoint'),
    $waiting= $('#waiting'),
    $koopLine= $('#koopLine'),
    $currentPointLine= $('#currentPointLine'),
    $totalPointLine= $('#totalPointLine')
;

//$('#game').hide();
$sidebar.hide();
$leftButton.hide();


function nextMap() {
    if (ws.readyState === 1) {
        ws.send('nextMap');
     //   ws.send('visibility');
    }
    else {
        setTimeout(function () {
            nextMap();
        }, 100);
    }
}

function button() {
    nextMap();
    $textfield.hide();
    $leftButton.html("Los");
    $leftButton.hide();
}

function trainingButton() {
    switch (tutstate) {
        case 0:
            if (($('input[name=gender]:checked').val() != null) && ($('#age')[0].checkValidity())) {
                ws.send("participant:" + $('input[name=gender]:checked').val() + "," + $('#age').val());
                $textfield.html(text.general0.text);
                tutstate++;
                ws.send('ping');
            }
            break;
        case 1:
            $game.append(smallGrid);
            $game.show();
            $textfield.html(text.general1.text);
            tutstate++;
            $trainingButton.hide();
            ws.send('ping');
            break;
        case 2:
            if (!(position)) {
                reveal(position);
                updateGrid('smallGrid');
                $textfield.html(text.general2.text);
                ws.send('ping');
            }
            tutstate++;
            $waiting.hide();
            $koopLine.hide();
            $currentPointLine.hide();
            $totalPointLine.hide();
            $sidebar.show();
            $trainingButton.hide();
            break;

        case 3:
            $textfield.html(text.general3.text);
            tutstate++;
            clickable = false;
            $trainingButton.show();
            ws.send('ping');
            break;
        case 4:
            $textfield.html(text.search0.text);
            tutstate++;
            $('.grid').hide();
            $game.show();
            $trainingButton.show();
            ws.send('ping');
            break;
        case 5:
            $textfield.html(text.search1.text);
            tutstate++;
            ws.send('ping');
            break;
        case 6: //gamecase
            //start game
            $('.grid').remove();
            $game.append(grid2D);
            $game.show();
            $textfield.hide();
            $trainingButton.hide();
            nextMap();
            ws.send('ping');
            break;
        case 7:
            $('.grid').hide();
            $textfield.html(text.stop0.text);
            tutstate++;
            ws.send('ping');
            break;
        case 8:
            $textfield.html(text.stop1.text);
            tutstate++;
            ws.send('ping');
            break;
        case 9://gamecase
            //startgame
            $('.grid').remove();
            $game.append(grid1D);
            $textfield.hide();
            $trainingButton.hide();
            nextMap();
            ws.send('ping');
            break;
        case 10:
            $('.grid').hide();
            $textfield.html(text.both0.text);
            $trainingButton.show();
            tutstate++;
            ws.send('ping');
            break;
        case 11:
            $textfield.html(text.both1.text);
            tutstate++;
            ws.send('ping');
            break;
        case 12://gamecase
            //startgame
            $('.grid').remove();
            $game.append(grid2D);
            $textfield.hide();
            $trainingButton.hide();
            nextMap();
            ws.send('ping');
            break;
    }
}

ws.onmessage = function (evt) {

    console.log(JSON.parse(evt.data));
    var msg = JSON.parse(evt.data);
    switch (msg.id) {
        case "VisibilityMap":
            visibilityMap = msg;
            position = visibilityMap.position;
            sendPosition = visibilityMap.position;
            if (visibilityMap.structure.length === map.edgeSize) {
                visibilityMap.dimension = 1;
                updateGrid("grid1D");
            } else {
                visibilityMap.dimension = 2;
                updateGrid("grid2D");
            }

            break;

        case "New":
            $sidebarTable.show();
            $('.grid').hide();
            //resetGrid('grid1D');
            //resetGrid('grid2D');
            $("#currentPoint").html(0)
            clickable = false;
            break;

        case "Landscape":
            map = msg;

            if (map.structure.length === map.edgeSize) {
                map.dimension = 1;
              //  resetGrid('grid1D');
            } else {
                map.dimension = 2;
              //  resetGrid('grid2D');
            }
            clickable = true;
            $('.grid').show();
            break;

        case "State":
            var stage = state.stage || 0;
            var level = state.level || 0;
            state = msg;
            var currentState = state.levelTypes[state.level];
            //end experiment
            if (level > state.level) {
                clickable = false;
                $sidebar.hide();
                $game.hide();
                $totalPoint.html(parseInt($currentPoint.html()) + parseInt($totalPoint.html()));
                $textfield.html("Das Experiment ist beendet. " +
                "Du hast " + $("#totalPoint").html() +" Punkte gesammelt. " +
                "Das entspricht einem Bonus von "+(parseInt($("#totalPoint").html()) * .0015).toFixed(2)+ " Euro.");

                $textfield.show();
                $trainingButton.hide()

            }
            else if (stage !== state.stage) {
                clickable = false;
                console.log("state!=state.stage");
                $totalPoint.html(parseInt($currentPoint.html()) + parseInt($totalPoint.html()));
                $textfield.html("Du hast " + $("#currentPoint").html() + " Punkte in diesem Level erreicht. " +
                    "Insgesamt beträgt deine Punktzahl " + $("#totalPoint").html() + " Punkte. " +
                    "Jetzt beginnt der nächste Teil des Experiments, " +
                    "mit einer kurzen Einführung.");
                $textfield.show();
                $sidebarTable.hide();

                $trainingButton.html("Weiter");
                $trainingButton.show();
                tutstate++;

            }
            else if (level !== state.level) {
                clickable = false;
                console.log("level !== state.level");
                $totalPoint.html(parseInt($currentPoint.html()) + parseInt($totalPoint.html()));
                $textfield.html("Du hast " + $("#currentPoint").html() +  " Punkte in diesem Level erreicht. " +
                    "Insgesamt beträgt deine Punktzahl " + $("#totalPoint").html() + " Punkte. " +
                    "Du kannst jetzt das nächste Level beginnen.");
                $textfield.show();
                $sidebarTable.hide();

                $leftButton.html("Weiter");
                $leftButton.show();
                $trainingButton.hide();
            }

            else {
                clickable = true;
                $stepTotal.html(currentState.stepsPlayed);
                $koop.html('Nein');
                $copyAllowed.html('Nein');
                $copyAllowedLine.hide();
                $step.html(state.step + 1);
                $level.html(state.level+1);
            }
            break;
    }
};

    ws.onclose = function () {
        alert("Closed!");
    };
    ws.onerror = function (err) {
        alert("Error: " + err);
    };

    function makeRandomArray(size) {
        var array = new Array(size);
        for (var i = 0; i < array.length; i++) {
            array[i] = getRandomIntInclusive(0, 50);
        }
        return array;
    }

    function getRandomIntInclusive(min, max) {
        min = Math.ceil(min);
        max = Math.floor(max);
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }


