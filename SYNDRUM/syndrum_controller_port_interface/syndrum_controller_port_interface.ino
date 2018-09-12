#include <MIDI.h>  // Add the "MIDI Library" library by Forty Seven Effects

//Create an instance of the library with default name, serial port and settings (RX and TX pins)
MIDI_CREATE_DEFAULT_INSTANCE();

void setup() {
  
  //Sets up all the outputs on the arduino
  for(int i=2; i < 14; i++) 
  {
    pinMode (i, OUTPUT);
    digitalWrite (i, HIGH);
  }
  
  MIDI.begin(10); // Initialize the Midi Library.
  // MIDI.begin(MIDI_CHANNEL_OMNI) sets it to listen to all channels.. MIDI.begin(1) would set it 
  // to respond to notes on channel 1 only.
  MIDI.setHandleNoteOn(MyHandleNoteOn); // This is important!! This command
  // tells the Midi Library which function you want to call when a NOTE ON command
  // is received. In this case it's "MyHandleNoteOn".
  MIDI.setHandleNoteOff(MyHandleNoteOff); // This command tells the Midi Library 
  // to call "MyHandleNoteOff" when a NOTE OFF command is received.
}

void loop() { // Main loop
  //digitalWrite(13,LOW);
  MIDI.read(); // Continuously check if Midi data has been received.
}


void MyHandleNoteOn(byte channel, byte pitch, byte velocity) { 

  digitalWrite(13,LOW);

   switch(pitch)
  {
    case 35: // KICK
    digitalWrite(2,LOW);  //Turn note on
    break;

    case 42: // HIGH HAT
    digitalWrite(3,LOW);  //Turn note on
    break;

    case 45: // LOW TOM
    digitalWrite(4,LOW);  //Turn note on
    break;

    case 50: // HIGH TOM
    digitalWrite(5,LOW);  //Turn note on
    break;

    case 38: // SNARE
    digitalWrite(6,LOW);  //Turn note on
    break;

    default:
    break;
   }
  
}


void MyHandleNoteOff(byte channel, byte pitch, byte velocity) { 

  digitalWrite(13,HIGH);
  
    switch(pitch)
  {
    case 35: // KICK
    digitalWrite(2,HIGH);  //Turn note off
    break;

    case 42: // HIGH HAT
    digitalWrite(3,HIGH);  //Turn note off
    break;

    case 45: // LOW TOM
    digitalWrite(4,HIGH);  //Turn note off
    break;

    case 50: // HIGH TOM
    digitalWrite(5,HIGH);  //Turn note off
    break;

    case 38: // SNARE
    digitalWrite(6,HIGH);  //Turn note off
    break;

    default:
    break;
  }
  
}
