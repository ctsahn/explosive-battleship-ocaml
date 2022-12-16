let secondClick = false;
let startSquare = "usercell55";
console.log("asgasdg");
async function handleShipPlacement(cellid, formid, ready) {

    if (document.getElementById(formid) && !ready  ) {

        document.getElementById(cellid).style.backgroundColor = "purple";
        document.getElementById(formid).submit();

    }
}

function loadColors() {
    let userBoardStatus = document.getElementById("user-board-status").innerHTML;

    for (let i = 0; i < 100; i++) {
        if (userBoardStatus[i] == 2) {
            document.getElementById("usercell" + i).style.backgroundColor = "purple";
            document.getElementById("usercell" + i).innerHTML = "";
        }
        if (userBoardStatus[i] == 1) {
            document.getElementById("usercell" + i).style.backgroundColor = "lightgray";
            document.getElementById("usercell" + i).innerHTML = "";
        }
        if (userBoardStatus[i] == 5) {
            document.getElementById("usercell" + i).style.backgroundColor = "black";
            document.getElementById("usercell" + i).innerHTML = "";
        }
    }

    // highlight lengths of previous ships you've placed
    let shipStatus = document.getElementById("ship-status").innerHTML;

    for (let i = 0; i < shipStatus.length; i++) {
        document.getElementById("length" + shipStatus.charAt(i)).style.color = "purple";
    }

    // highlight length of current ship you place
    let shipSize = document.getElementById("placed-ship-size").innerHTML;

    // not 0
    if (shipSize > 0) {
        document.getElementById("length" + shipSize).style.color = "purple";
    }

}







