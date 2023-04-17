package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
)

type SearchResult struct {
	TotalCount int `json:"total_count"`
	Items      []struct {
		Name        string `json:"name"`
		Path        string `json:"path"`
		Url         string `json:"url"`
		Repository  struct {
			Name string `json:"name"`
			Owner struct {
				Login string `json:"login"`
			} `json:"owner"`
		} `json:"repository"`
	} `json:"items"`
}

func main() {
	if len(os.Args) != 3 {
		fmt.Println("Usage: github-search [file-extension] [search-term]")
		return
	}

	// Replace with your personal access token
	accessToken := "YOUR_GITHUB_PERSONAL_ACCESS_TOKEN"
	fileExtension := os.Args[1]
	searchTerm := os.Args[2]

	searchQuery := url.QueryEscape(fmt.Sprintf("%s in:file language:%s", searchTerm, fileExtension))
	apiURL := fmt.Sprintf("https://api.github.com/search/code?q=%s&access_token=%s", searchQuery, accessToken)

	resp, err := http.Get(apiURL)
	if err != nil {
		fmt.Printf("Error fetching data from GitHub API: %v\n", err)
		return
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error reading response body: %v\n", err)
		return
	}

	var searchResult SearchResult
	err = json.Unmarshal(body, &searchResult)
	if err != nil {
		fmt.Printf("Error unmarshalling JSON: %v\n", err)
		return
	}

	fmt.Printf("Total results: %d\n", searchResult.TotalCount)
	for _, item := range searchResult.Items {
		fmt.Printf("Repo: %s/%s, Path: %s, URL: %s\n", item.Repository.Owner.Login, item.Repository.Name, item.Path, item.Url)
	}
}
