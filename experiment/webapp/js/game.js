/**
 * Created by Yahosseini on 20.12.2016.
 */
"use strict";
var grid2D = clickableGrid(63, 63, createCallback2D, 'grid2D');

var smallGrid = clickableGrid(10, 10, createCallbackSmall, 'smallGrid');

var grid1D = clickableGrid(1, 63, createCallback1D, 'grid1D');

function clickableGrid(rows, cols, callback, id) {
    var i = 0;
    var grid = document.createElement('table');
    grid.className = 'grid';
    grid.class = 'grid';
    grid.id = id;
    for (var r = 0; r < rows; ++r) {
        var tr = grid.appendChild(document.createElement('tr'));
        for (var c = 0; c < cols; ++c) {
            i = c + (rows * r);
            var cell = tr.appendChild(document.createElement('td'));
            cell.innerHTML = '\xa0\xa0';
            cell.addEventListener('click', callback(cell, r, c, i), false);
        }
    }
    return grid;
}

function createCallbackSmall(el, row, col, i) {
    return function () {
        if (!(position) || isNeighbourOf(i, sendPosition)) {
            console.log(el + " " + row + " " + col + " " + i);
            position = i;
            lastClicked = el;
            reveal(position);
            updateGrid('smallGrid');
            trainingButton();
        }
    }
}

function createCallback2D(el, row, col, i) {
    return function () {
        if ((clickable === true) && (state.step < state.levelTypes[state.level].stepsPlayed) && (!(position) || isNeighbourOf(i, sendPosition))) {
            console.log(el + " " + row + " " + col + " " + i);
            position = i;
            ws.send("position:" + position);
            console.log("position:" + position);
            lastClicked = el;
            clickable = false;
            //updateGrid('grid2D');
        }
    }
}

function createCallback1D(el, row, col, i) {
    return function () {
        if ((clickable === true)&& (state.step < state.levelTypes[state.level].stepsPlayed) && (!(position) || isNeighbourOf(i, sendPosition))) {
            console.log(el + " " + row + " " + col + " " + i);
            position = i;
            ws.send("position:" + position);
            console.log("position:" + position);
            lastClicked = el;
            clickable = false;
            //updateGrid('grid1D');
        }
    }
}


function updateGrid(id) {
    var gridElements = $('#'+id+' td' )
    var doublespace = '\xa0\xa0';
    for(var i = 0; i< gridElements.length; i++) {
        var v = gridElements[i];
        v.className = '';
        switch (visibilityMap.structure[i]) {
            case 0:
                v.innerHTML = doublespace;
                break;
            case 1:
                v.innerHTML = ((doublespace + map.structure[i]).substr(-2));
                v.className = v.className + ' other';
                break;
            case 2:
                v.innerHTML = ((doublespace + map.structure[i]).substr(-2));
                v.className = v.className + ' revealed';
                break;
        }
        if (sendPosition === i || position === i) {
            v.className = v.className+' position';
            if (state.stage === 0) {
                $currentPoint.html(Math.max(parseInt($currentPoint.html()),
                    map.structure[sendPosition]));
            }
            else {
                $currentPoint.html(map.structure[sendPosition]);
            }

        }
    }

}

function resetGrid(id) {
    var doublespace = '\xa0\xa0';
    var gridElements = $('#'+id+' td' )

    for(var i = 0; i< gridElements.length; i++) {
        var v = gridElements[i];
        v.className = '';
        v.innerHTML = doublespace;
    }
    updateGrid(id);
}

function reveal(position) {
    var neighbours = getNeighbours(position);
    visibilityMap.structure = new Array(100).fill(0);
    for (var i = 0; i < neighbours.length; i++) {
        visibilityMap.structure[neighbours[i]] = 2;
    }
    sendPosition = position;
}

function isNeighbourOf(newPosition, oldPosition) {
    return getNeighbours(oldPosition).indexOf(newPosition) !== -1;
}

function getNeighbours(position) {
    if (visibilityMap.dimension === 1) {
        return getNeighbours1D(position)
    }
    else {
        return getNeighbours2D(position)
    }
}

function getNeighbours1D(position) {
    var neighbours = [];
    neighbours.push(position);
    neighbours.push(position + 1);
    neighbours.push(position - 1);
    for (var i = 0; i < neighbours.length; i++) {
        if (neighbours[i] < 0 || neighbours[i] >= visibilityMap.structure.length) {
            neighbours.splice(i, 1);
        }
    }
    return neighbours;
}

function getNeighbours2D(position) {
    var neighbours = [];
    if ((position % visibilityMap.edgeSize) === 0) {
        neighbours.push(position + 1 - visibilityMap.edgeSize);
        neighbours.push(position + 1);
        neighbours.push(position + 1 + visibilityMap.edgeSize);
    }
    //right border
    else if ((position % visibilityMap.edgeSize) === (visibilityMap.edgeSize - 1)) {
        neighbours.push(position - 1 - visibilityMap.edgeSize);
        neighbours.push(position - 1);
        neighbours.push(position - 1 + visibilityMap.edgeSize);
    }
    //not right or left border
    else {
        neighbours.push(position + 1 - visibilityMap.edgeSize);
        neighbours.push(position + 1);
        neighbours.push(position + 1 + visibilityMap.edgeSize);
        neighbours.push(position - 1 - visibilityMap.edgeSize);
        neighbours.push(position - 1);
        neighbours.push(position - 1 + visibilityMap.edgeSize);
    }
    //upper
    neighbours.push(position + visibilityMap.edgeSize);
    //lower
    neighbours.push(position - visibilityMap.edgeSize);
    neighbours.push(position);
    for (var i = 0; i < neighbours.length; i++) {
        if (neighbours[i] < 0 || neighbours[i] >= visibilityMap.structure.length) {
            neighbours.splice(i, 1);
        }
    }
    return neighbours;
}