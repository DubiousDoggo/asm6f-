
#ifndef NONSTDLIB_HPP
#define NONSTDLIB_HPP

#include <algorithm>
#include <string>
#include <iostream>

namespace nonstd
{

template <typename InputIterator, typename ForwardIterator>
InputIterator find_first_not_of(InputIterator first1, InputIterator last1, ForwardIterator first2, ForwardIterator last2)
{
  for (; first1 != last1; ++first1)
  {
    ForwardIterator iter = first2;
    for (; iter != last2; ++iter)
      if (*first1 == *iter)
        break;
    if (iter == last2)
      return first1;
  }
  return last1;
}

template <class T, class ForwardIt>
ForwardIt eat_characters(const ForwardIt &begin, const ForwardIt &end, const std::basic_string<T> characters)
{
  return nonstd::find_first_not_of(begin, end, characters.begin(), characters.end());
}

std::string to_upper(const std::string &str)
{
  std::string r;
  std::transform(str.cbegin(), str.cend(), std::back_insert_iterator(r), ::toupper);
  return r;
}

// Returns the next word from the range and advances begin iterator.
std::string getword(std::string::const_iterator &begin, const std::string::const_iterator &end, const std::string &delimiters)
{
  auto start = nonstd::find_first_not_of(begin, end, delimiters.begin(), delimiters.end());
  auto stop = std::find_first_of(start, end, delimiters.begin(), delimiters.end());
  begin = stop;
  return std::string(start, stop);
}

// Returns the next word from the range and advances begin iterator.
// The start of the returned string will be the first character not in delimiters.
// The begining iterator will be advanced from the start of the word to either the
// first character in delimiters, or the first character in trailing
// delimiters that follows a character not in the trailing delimiters.
// This is similar to the functionality of `strtok`. The returned string ends
// at the character before the begin iterator, after advancing.
// If no such delimiter is found, begin will be advanced to the end iterator.
// TODO document examples
std::string getword(std::string::const_iterator &begin, const std::string::const_iterator &end,
                    const std::string &delimiters, const std::string &trailing_delimiters)
{
  const auto start = nonstd::find_first_not_of(begin, end, delimiters.cbegin(), delimiters.cend());
  auto stop = std::find_first_of(start, end, delimiters.cbegin(), delimiters.cend());
  begin = nonstd::find_first_not_of(start, stop, trailing_delimiters.cbegin(), trailing_delimiters.cend());
  begin = std::find_first_of(begin, stop, trailing_delimiters.cbegin(), trailing_delimiters.cend());
  return std::string(start, begin);
}

} // namespace nonstd

#endif