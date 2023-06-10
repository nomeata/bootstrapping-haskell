#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
% Last Modified: Wed Jul 19 12:03:26 1995
% Darren J Moffat <moffatd@dcs.gla.ac.uk>
\section[LibReadline]{GNU Readline Library Bindings}

\begin{code}
#endif

#include "rtsdefs.h"

#include "ghcReadline.h" /* to make sure the code here agrees...*/

/*
Wrapper around the callback mechanism to allow Haskell side functions
to be callbacks for the Readline library.

The C function $genericRlCback$ puts the cback args into global
variables and enters the Haskell world through the $haskellRlEntry$
function. Before exiting, the Haskell function will deposit its result
in the global variable $rl_return$.
*/

I_ current_narg, rl_return, current_kc;

char* rl_prompt_hack;

StgStablePtr haskellRlEntry;
StgStablePtr cbackList;


I_
genericRlCback (I_ narg, I_ kc)
{
  current_narg = narg;
  current_kc = kc;
  
  performIO(haskellRlEntry);

  return rl_return;
}
