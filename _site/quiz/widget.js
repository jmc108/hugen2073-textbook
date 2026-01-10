const QUIZ_API_BASE = "http://127.0.0.1:8000";

async function gradeAnswer(chapterId, questionId, answer) {
  const r = await fetch(`${QUIZ_API_BASE}/grade`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ chapter_id: chapterId, question_id: questionId, answer })
  });
  if (!r.ok) throw new Error(`HTTP ${r.status}`);
  return await r.json();
}

async function loadLocalChapterQuestion(chapterId, questionId) {
  // Loads the prompt from the static JSON in your Quarto site
  const r = await fetch(`/quiz/chapters/${chapterId}.json`);
  if (!r.ok) throw new Error(`Cannot load chapter JSON: ${chapterId}`);
  const data = await r.json();
  const q = data.questions.find(x => x.question_id === questionId);
  if (!q) throw new Error(`Question not found: ${questionId}`);
  return q;
}

document.addEventListener("DOMContentLoaded", async () => {
  for (const box of document.querySelectorAll(".quiz-box")) {
    const chapterId = box.dataset.chapter;
    const questionId = box.dataset.question;

    const promptEl = box.querySelector(".quiz-prompt");
    const answerEl = box.querySelector(".quiz-answer");
    const submitEl = box.querySelector(".quiz-submit");
    const resultEl = box.querySelector(".quiz-result");

    try {
      const q = await loadLocalChapterQuestion(chapterId, questionId);
      promptEl.textContent = q.prompt;
    } catch (e) {
      promptEl.textContent = `Quiz failed to load: ${e.message}`;
      continue;
    }

    submitEl.addEventListener("click", async () => {
      resultEl.textContent = "Checking...";
      try {
        const data = await gradeAnswer(chapterId, questionId, answerEl.value);

        if (data.passed) {
          resultEl.innerHTML = `<b>✅ Pass</b> (score ${data.score.toFixed(2)})<br>${data.feedback}`;
        } else {
          const miss = (data.missing_points || []).map(s => `<li>${s}</li>`).join("");
          resultEl.innerHTML = `<b>❌ Not yet</b> (score ${data.score.toFixed(2)})<br>${data.feedback}<ul>${miss}</ul>`;
        }
      } catch (e) {
        resultEl.textContent = `Error: ${e.message}`;
      }
    });
  }
});
