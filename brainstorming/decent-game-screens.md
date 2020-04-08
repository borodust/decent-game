# My MD format seems to be off with tables for me, so use RTF document in same folder instead. 

** **** Breakdown of game screens / key scenes. To be Discussed.** _See table below for information on (game-elements)_

Key Mandatory Screens:

**I) Start  / Load Screen:  ****Main Load Screen**** (main-load-screen)** FS-M

Ia) Text Info explaining plot (game-intro)(pick-gear) FS-CS or FS-M

**II) on ESC  -  Main Escape Menu (main-esc-menu)** FS-M

**III)** **Final Game Outcome (final-outcome)** FS-CS

**IV)** **End Credits (end-credits)****.**

! If, due to limited resources, story development in-game has to be abandoned, a minimal story can be done with 2 info screens –  1a) and III)

Formatting:

Presumed Mandatory, can&#39;t be removed  screens or scenes below are in bold.

Presumed Mandatory, CAN be removed or joined with other events screens or scenes below are in bold italic.

See script-draft for some other notes https://gitlab.com/readeval/decent-game/-/blob/master/brainstorming/SCRIPT-DRAFT.md

Abbreviations used summary:

_(OSD)_  – on-screen display, partial display of information during game, text bar and icon image.  (see script-draft).

 (_TBD)_– to be discussed

(FS-CS)Full-screen cut-screens with in-code editable text and insert-able image. These will be used for informing the player and for end credits.

(FS-M) Full-screen menu (or up to AD how it is implemented)

(AD)  - Art Director

(S/F) – Sound Effect

(G) Graphic (can be animation or zoom-in on existing asset, or just existing asset, up to AD)

(M) Music

_General interface questions_ –

Keys and mouse or  keys only? TODO summary of in-game keys, key combos, menu switch / pause  in separate table.

Non-mandatory text info can be exited with ESC, text prompts require choice to be made. If player must make choice in menu, text info – &quot;You must make a choice&quot;.

| Visual | Description | Assets  / notes |
| --- | --- | --- |
| Great visuals representing the game, up to AD (art-director) | Screen: I **Main Load Screen****  (main-load-screen)**On game start Type: Image (or animation )  and menuText: GAME NAMEMenu option: New GameMenu option: Continue Menu option: LoadMenu option: Credits  | Graphic – Image or  optional Animation  Optional – switch to demo animation video after 3 seconds player in-action ( exit on ESC ) Music, M: Custom Load music or intro / beginning music**Sound-effect, S/F: Menu choice** |
| Up to AD | Screen: II **Main Escape Menu (main-esc-menu)**Menu option: New GameMenu option: Continue Menu option: LoadMenu option: Main MenuDescription: What happens when palyer hits escape key in-game | M: Optional music |
| To be Discussed, TBD | Scene / Event: **Game Plot Intro**  **(game-intro)**1 **Information to player at start of game**** (info-on-start)**Text: To be addedSummary: Explains what is going on.  | Up to Art DirectorM: optional low feeling audio, express dying. |
| TBD, up to ADMinimal: zoom of gear asset, text description, text prompt.Otherwise interface can be as complicated as AD wants. | **Scene / Event Prompt to pick alien or cyborg gear (pick-gear)**In-game Menu: Pick alien or cyborg gear, or don&#39;t pick any for hard-core human path players.Idea: unarmed human hero encounters enemy, is wounded. As Hero is dying, a choice to pick gear appears. Minimal idea: Text describing hero is weak, option to pick gear. | Up to Art Director |
| Zoom on existing boss asset or custom portrait ( see shovel knight OSD) | Scene / Event: Boss-taunts (boss-taunts)Summary: OSD, boss says crazy annoying things throughout game?Discuss: Boss persona, proposals coming | Up to Art Director |
| Optional close-up of transformed hero, cyborg or alien. Can be separate art or zoom on existing asset. | Screen, FS-CS: **Irreversible Transformation (transformation)**Summary: Hero undergoes transformation to final alien or cyborg form after enough Text: explains what has happened | Graphic – Image or  optional Animation Optional – special music, dramatic |
| Optional visual of NPC needing help from hero | Scene / Event or FS-CS: NPC needs help, optional  (npc-help)Summary: to be expanded in detailHero has passive choice of running by or helping NPC by engaging enemies that attack NPC.  Text: OSD ?Discuss: Mechanics | Graphic – Image or  optional Animation Optional – special music, dramatic |
| Glad NPC | Scene/Event: Grateful NPC (npc-grateful)  | Glad NPC audio. |
|   | Screen, FS-CS: **Boss monologue before final battle (boss-monologue)**Description: Plot twist, boss explains that hero is not that much of a hero, and that boss is not insane. Hero keeps respawning and losing memory, but it is hero and others like him that caused the world to collapse. TBD: Should there be discussion tree in text | G:Optional zoom in on bossG:Optional Boss transformation into bigger boss |
|   | Scene / event: **Boss Battle (boss-battle)** |   |
| TBD | Scene / Event: **Engage Self-destruct Button (self-destruct)** |   |
|   | Screen: III **Final Game Outcome (final-outcome)**FS-CSDescription: Shows consequence of player choices **.** | Full-screen, contains image and text , optional video |
| Wrap-up story visually. Hero triumphant, possibly with NPCs celebrating. Or zoom in on hero. Up to AD | Screen: _Transition to End Credits (transition-credits)_ | Graphic – Image or  optional Animation Up to AD |
| Any great images from game or possibly development sketches, if they&#39;re cool | Screen: IV **End Credits (end-credits)**Type: Full-Screen cut-screen, Text and ImageText: Developer creditsMenu: ( Main menu on ESC )  | Graphic – Image or  optional Animation, up to ADText: Credits Text Info  |

Characters / Units

| Visual | Desc | Assets |
| --- | --- | --- |
| Up to AD | **Hero** , playable Has stages of progressive transformation.Has upgrade / gear select menu | S/F: All attacks, Damage taken, Death, Heal. Optional Taunt.   |
|   | Final Boss Possibly attacks by calling monster swarms (like opening a door through which monsters come) Text: Possibly has taunt texts before fight, dialogue menu before fight, statement after fight | S/F: All attacks, Damage taken, Death.  Optional Taunt, optional heal. |
|   |   |   |
| Up to AD | NPC, Humans in distress | S/F: Ask for help, happy when saved |
|   |   |   |
| Up to AD | **Enemies** 1-4 | S/F: Attack, death |
|   | Alien 1 | S/F: Attack, death |
|   | Alien 2 | S/F: Attack, death |
|   | Cyborg 1 | S/F: Attack, death |
|   | Cyborg 2 | S/F: Attack, death |
|   |   |   |