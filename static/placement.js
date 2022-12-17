function handleShipPlacement(cellid, formid, ready) {

    if (document.getElementById(formid) && !ready  ) {
        document.getElementById(cellid).style.backgroundColor = "purple";  // color the clicked cell to be purple
        document.getElementById(formid).submit(); // submit corresponding cell form for placement (POST request)
    }
}

function loadColors(userBoardStatus, shipStatus) {
    

    for (let i = 0; i < 100; i++) {
        // ship
        if (userBoardStatus[i] == 2) {
            document.getElementById("usercell" + i).style.backgroundColor = "purple";
            document.getElementById("usercell" + i).innerHTML = "";
        }
        // ship's radius
        if (userBoardStatus[i] == 1) {
            document.getElementById("usercell" + i).style.backgroundColor = "lightgray";
            document.getElementById("usercell" + i).innerHTML = "";
        }
        // mine
        if (userBoardStatus[i] == 5) {
            document.getElementById("usercell" + i).style.backgroundColor = "black";
            document.getElementById("usercell" + i).innerHTML = "";
        }
    }
    // highlight lengths of previous ships you've placed
    for (let i = 0; i < shipStatus.length; i++) {
        document.getElementById("length" + shipStatus.charAt(i)).style.color = "purple";
        document.getElementById("length" + shipStatus.charAt(i)).innerHTML = document.getElementById("length" + shipStatus.charAt(i)).innerHTML + " - placed!"; 
    }

}







