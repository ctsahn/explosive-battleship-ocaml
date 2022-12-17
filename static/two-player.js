let disable = false; //disable multiple clicks
let bombSet = false;
function handleClick(cellid, formid, currentTurn,gameOver) {

  // only allow clicks when it is your turn, and make sure a form exists in order for click to occur
  if (((currentTurn.includes("player1") && cellid.includes("player2")) || (currentTurn.includes("player2") && cellid.includes("player1"))) && !disable && document.getElementById(formid) && !gameOver ) {
    document.getElementById(cellid).style.backgroundColor = "red";
    if (bombSet){
        document.getElementById(formid.replace("form","input")).value = "bomb" + document.getElementById(formid.replace("form","input")).value;
    }
    document.getElementById(formid).submit();
    disable = true;
  }
}


function loadTwoPlayer(player1BoardStatus, player2BoardStatus, gameOver) {

  for (let i = 0; i < 100; i++) {

    // miss
    if (player1BoardStatus[i] == 1) {
      document.getElementById("player1cell" + i).innerHTML = "•";
    }
    
    // only reveal ships if game is over
    else if (player1BoardStatus[i] == 2 && gameOver ) {
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
    // only reveal mine if game is over
    else if (player1BoardStatus[i] == 5 && gameOver ) {
      document.getElementById("player1cell" + i).style.backgroundColor = "gray";
    }
    // mine hit
    else if (player1BoardStatus[i] == 6  ) {
      document.getElementById("player1cell" + i).style.backgroundColor = "black";
      document.getElementById("player1cell" + i).innerHTML = "";
    }

    // miss
    if (player2BoardStatus[i] == 1) {
      document.getElementById("player2cell" + i).innerHTML = "•";
    }
    // only reveal ships if game is over
    else if (player2BoardStatus[i] == 2 && gameOver) {
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
    // only reveal mine if game is over
    else if (player2BoardStatus[i] == 5 && gameOver) {
      document.getElementById("player2cell" + i).style.backgroundColor = "gray";
    }
    // mine hit
    else if (player2BoardStatus[i] == 6  ) {
      document.getElementById("player2cell" + i).style.backgroundColor = "black";
      document.getElementById("player2cell" + i).innerHTML = "";
      
    }

  }

}
