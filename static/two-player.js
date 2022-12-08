let disable = false; //disable multiple clicks
async function handleClick(cellid, formid, currentTurn) {
  // only allow clicks when it is your turn, and make sure a form exists in order for click to occur
  if (((currentTurn.includes("player1") && cellid.includes("player2")) || (currentTurn.includes("player2") && cellid.includes("player1"))) && !disable && document.getElementById(formid)) {
    document.getElementById(cellid).style.backgroundColor = "red";
    document.getElementById(formid).submit();
    disable = true;
  }

}



function loadColors() {

  let player1BoardStatus = document.getElementById("player1-board-status").innerHTML;
  let player2BoardStatus = document.getElementById("player2-board-status").innerHTML;
  for (let i = 0; i < 100; i++) {

    // miss
    if (player1BoardStatus[i] == 1) {
      document.getElementById("player1cell" + i).innerHTML = "•";
    }
    // ship
    else if (player1BoardStatus[i] == 2) {
      document.getElementById("player1cell" + i).style.backgroundColor = "blue";
    }
    // hit
    else if (player1BoardStatus[i] == 3) {
      document.getElementById("player1cell" + i).innerHTML = "X";
      document.getElementById("player1cell" + i).style.color = "red";
      document.getElementById("player1cell" + i).style.borderColor = "red";

    }
    // sunk
    else if (player1BoardStatus[i] == 4) {
      document.getElementById("player1cell" + i).style.backgroundColor = "red";
      document.getElementById("player1cell" + i).style.color = "red";
      document.getElementById("player1cell" + i).style.borderColor = "red";
    }

    // miss
    if (player2BoardStatus[i] == 1) {
      document.getElementById("player2cell" + i).innerHTML = "•";
    }
    // ship
    else if (player2BoardStatus[i] == 2) {
      document.getElementById("player2cell" + i).style.backgroundColor = "blue";
    }
    // hit
    else if (player2BoardStatus[i] == 3) {

      document.getElementById("player2cell" + i).innerHTML = "X";
      document.getElementById("player2cell" + i).style.color = "red";
      document.getElementById("player2cell" + i).style.borderColor = "red";
    }
    // sunk
    else if (player2BoardStatus[i] == 4) {
      document.getElementById("player2cell" + i).style.backgroundColor = "red";
      document.getElementById("player2cell" + i).style.color = "red";
      document.getElementById("player2cell" + i).style.borderColor = "red";
    }

  }

}
loadColors(); // we want to call this every time we load a page