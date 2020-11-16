import sys
import requests
import bs4

def mod_inv(n, p):
	t = 0
	nt = 1
	r = p
	nr = n % p
	while nr != 0:
		quot = r // nr
		tmp = nt; nt = t - quot * nt; t = tmp
		tmp = nr; nr = r - quot * nr; r = tmp
	if t < 0:
		t += p
	return t

def binomial(n, k, p):
	m = min(k, n - k)
	num = 1
	denom = 1
	for i in range(1, m + 1):
		num = num * (n + 1 - i) % p
		denom = denom * i % p
	return num * mod_inv(denom, p) % p


if __name__ == "__main__":
	if (len(sys.argv)) < 2:
		exit("You have to provide the website that contains the game as an argument!\n")
	elif (len(sys.argv)) > 2:
		exit("Sorry, one argument only!\n")	

	url = sys.argv[1]
	s = requests.Session()
	page = s.get(url)

	while True:
		soup = bs4.BeautifulSoup(page.content, 'html.parser')
		N = soup.find("span", id="N").text
		K = soup.find("span", id="K").text
		P = soup.find("span", id="P").text
		answer = binomial(int(N), int(K), int(P))
		question = soup.find('span', {'class':'question'}).text
		print("Round", question.split()[1] + ",", "C(" + N + "," + K + ") modulo", P)
		print("Answer:",answer)

		page = s.post(url, {'answer': answer})
		soup = bs4.BeautifulSoup(page.content, 'html.parser')
		right = soup.find('p', {'class':'right'})
		wrong = soup.find('p', {'class':'wrong'})
		if right:
			print(right.text)
		elif wrong:
			print(wrong.text)
		else:
			print(soup)
			exit("\n\nDidn't get a right/wrong reply!")

		finished = soup.find('span', {'class':'congratulations'})
		if finished:
			print(finished.text)
			print(" ".join([f.strip() for f in soup.findAll('p')[-1].text.split("\n")]))
			break
		else:
			page = s.post(url, {'again' : True})
