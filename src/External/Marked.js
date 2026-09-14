import { marked as parseMarkdown } from "marked";

export const marked = function (string) {
    return function () {
        return parseMarkdown(string, { smartypants: true, silent: true });
    };
};
export const setHTML = function (el) {
    return function (html) {
        return function () {
            el.innerHTML = html;
        };
    };
};
