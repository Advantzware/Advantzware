using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Text;
public partial class CodeLook : System.Web.UI.Page
{
    int tot = 0;
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
            Button3.Visible = false;
        }
        string str = @"<script language=javascript>
        function Tovalue()
            {  
            var text1=document.getElementById('Text1').value;
            var text2=document.getElementById('Text2').value;
            var text3=document.getElementById('Text3').value;
            var text4=document.getElementById('Text4').value;
            var text5=document.getElementById('Text5').value;
            var text6=document.getElementById('Text6').value;
            var text7=document.getElementById('Text7').value;
            var text8=document.getElementById('Text8').value;
            var text9=document.getElementById('Text9').value;
            var text10=document.getElementById('Text10').value;
            
            var text11=document.getElementById('Text11').value;
            var text12=document.getElementById('Text12').value;
            var text13=document.getElementById('Text13').value;
            var text14=document.getElementById('Text14').value;
            var text15=document.getElementById('Text15').value;
            var text16=document.getElementById('Text16').value;
            var text17=document.getElementById('Text17').value;
            var text18=document.getElementById('Text18').value;
            var text19=document.getElementById('Text19').value;
            var text20=document.getElementById('Text20').value;

            var text21=document.getElementById('Text21').value;
            var text22=document.getElementById('Text22').value;
            var text23=document.getElementById('Text23').value;
            var text24=document.getElementById('Text24').value;
            var text25=document.getElementById('Text25').value;
            var text26=document.getElementById('Text26').value;
            var text27=document.getElementById('Text27').value;
            var text28=document.getElementById('Text28').value;
            var text29=document.getElementById('Text29').value;
            var text30=document.getElementById('Text30').value;

            var ar= new Array(text1,text2,text3,text4,text5,text6,text7,text8,text9,text10,text11,text12,text13,text14,text15,text16,text17,text18,text19,text20,text21,text22,text23,text24,text25,text26,text27,text28,text29,text30);
            window.opener.update(ar);        
            window.close(); 
            }  
           </script>";

        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "Tovalue"))
        {
            ClientScript.RegisterClientScriptBlock(this.GetType(), "Tovalue", str.ToString());
        }

        Button3.Attributes.Add("onclick", "Tovalue();");
        
    }
    protected void select_click(object sender, EventArgs e)
    {
        StringBuilder str = new StringBuilder();
        StringBuilder str2 = new StringBuilder();
        StringBuilder str3 = new StringBuilder();
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool isChecked = ((CheckBox)row.FindControl("chkSelect")).Checked;

            if (isChecked)
            {
                tot = tot + 1;
                str.Append(GridView1.Rows[i].Cells[1].Text);
                str.Append(",");
                str2.Append(GridView1.Rows[i].Cells[2].Text);
                str2.Append(",");
                str3.Append(GridView1.Rows[i].Cells[3].Text);
                str3.Append(",");
            }
        }

        string ss = str.ToString();
        string s2=str2.ToString();
        string s3 = str3.ToString();

        if (tot == 1)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = "";
            Text3.Text = "";
            Text4.Text = "";
            Text5.Text = "";
            Text6.Text = "";
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = "";
            Text13.Text = "";
            Text14.Text = "";
            Text15.Text = "";
            Text16.Text = "";
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            Text22.Text = "";
            Text23.Text = "";
            Text24.Text = "";
            Text25.Text = "";
            Text26.Text = "";
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";

        }
        if (tot == 2)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = "";
            Text4.Text = "";
            Text5.Text = "";
            Text6.Text = "";
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = "";
            Text14.Text = "";
            Text15.Text = "";
            Text16.Text = "";
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            Text23.Text = "";
            Text24.Text = "";
            Text25.Text = "";
            Text26.Text = "";
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";

        }
        if (tot == 3)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = "";
            Text5.Text = "";
            Text6.Text = "";
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]); ;
            Text14.Text = "";
            Text15.Text = "";
            Text16.Text = "";
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            Text24.Text = "";
            Text25.Text = "";
            Text26.Text = "";
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";
        }
        if (tot == 4)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = "";
            Text6.Text = "";
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]) ;
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = "";
            Text16.Text = "";
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";
            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            
            Text25.Text = "";
            Text26.Text = "";
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";
        }
        if (tot == 5)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = "";
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = "";
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            Text26.Text = "";
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";
        }
        if (tot == 6)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Session["six"] = radioval[5];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = Convert.ToString(Session["six"]);
            Text7.Text = "";
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Session["six2"] = radioval2[5];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = Convert.ToString(Session["six2"]);
            Text17.Text = "";
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            Session["six3"] = radioval3[5];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            if (Convert.ToString(Session["six3"]) == "I")
            {
                Text26.Text = "25";
            }
            else
            {
                Text26.Text = "100";
            }
            Text27.Text = "";
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";
        }
        if (tot == 7)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Session["six"] = radioval[5];
            Session["seven"] = radioval[6];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = Convert.ToString(Session["six"]);
            Text7.Text = Convert.ToString(Session["seven"]);
            Text8.Text = "";
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Session["six2"] = radioval2[5];
            Session["seven2"] = radioval2[6];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = Convert.ToString(Session["six2"]);
            Text17.Text = Convert.ToString(Session["seven2"]);
            Text18.Text = "";
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            Session["six3"] = radioval3[5];
            Session["seven3"] = radioval3[6];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            if (Convert.ToString(Session["six3"]) == "I")
            {
                Text26.Text = "25";
            }
            else
            {
                Text26.Text = "100";
            }
            if (Convert.ToString(Session["seven3"]) == "I")
            {
                Text27.Text = "25";
            }
            else
            {
                Text27.Text = "100";
            }
            Text28.Text = "";
            Text29.Text = "";
            Text30.Text = "";
        }
        if (tot == 8)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Session["six"] = radioval[5];
            Session["seven"] = radioval[6];
            Session["eight"] = radioval[7];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = Convert.ToString(Session["six"]);
            Text7.Text = Convert.ToString(Session["seven"]);
            Text8.Text = Convert.ToString(Session["eight"]);
            Text9.Text = "";
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Session["six2"] = radioval2[5];
            Session["seven2"] = radioval2[6];
            Session["eight2"] = radioval2[7];
            
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = Convert.ToString(Session["six2"]);
            Text17.Text = Convert.ToString(Session["seven2"]);
            Text18.Text = Convert.ToString(Session["eight2"]);
            Text19.Text = "";
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            Session["six3"] = radioval3[5];
            Session["seven3"] = radioval3[6];
            Session["eight3"] = radioval3[7];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            if (Convert.ToString(Session["six3"]) == "I")
            {
                Text26.Text = "25";
            }
            else
            {
                Text26.Text = "100";
            }
            if (Convert.ToString(Session["seven3"]) == "I")
            {
                Text27.Text = "25";
            }
            else
            {
                Text27.Text = "100";
            }
            if (Convert.ToString(Session["eight3"]) == "I")
            {
                Text28.Text = "25";
            }
            else
            {
                Text28.Text = "100";
            }
            Text29.Text = "";
            Text30.Text = "";

        }
        if (tot == 9)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Session["six"] = radioval[5];
            Session["seven"] = radioval[6];
            Session["eight"] = radioval[7];
            Session["nine"] = radioval[8];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = Convert.ToString(Session["six"]);
            Text7.Text = Convert.ToString(Session["seven"]);
            Text8.Text = Convert.ToString(Session["eight"]);
            Text9.Text = Convert.ToString(Session["nine"]);
            Text10.Text = "";

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Session["six2"] = radioval2[5];
            Session["seven2"] = radioval2[6];
            Session["eight2"] = radioval2[7];
            Session["nine2"] = radioval2[8];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = Convert.ToString(Session["six2"]);
            Text17.Text = Convert.ToString(Session["seven2"]);
            Text18.Text = Convert.ToString(Session["eight2"]);
            Text19.Text = Convert.ToString(Session["nine2"]);
            Text20.Text = "";

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            Session["six3"] = radioval3[5];
            Session["seven3"] = radioval3[6];
            Session["eight3"] = radioval3[7];
            Session["nine3"] = radioval3[8];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            if (Convert.ToString(Session["six3"]) == "I")
            {
                Text26.Text = "25";
            }
            else
            {
                Text26.Text = "100";
            }
            if (Convert.ToString(Session["seven3"]) == "I")
            {
                Text27.Text = "25";
            }
            else
            {
                Text27.Text = "100";
            }
            if (Convert.ToString(Session["eight3"]) == "I")
            {
                Text28.Text = "25";
            }
            else
            {
                Text28.Text = "100";
            }
            if (Convert.ToString(Session["nine3"]) == "I")
            {
                Text29.Text = "25";
            }
            else
            {
                Text29.Text = "100";
            }
            Text30.Text = "";
        }
        if (tot == 10)
        {
            string[] radioval = ss.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            Session["four"] = radioval[3];
            Session["five"] = radioval[4];
            Session["six"] = radioval[5];
            Session["seven"] = radioval[6];
            Session["eight"] = radioval[7];
            Session["nine"] = radioval[8];
            Session["ten"] = radioval[9];
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            Text4.Text = Convert.ToString(Session["four"]);
            Text5.Text = Convert.ToString(Session["five"]);
            Text6.Text = Convert.ToString(Session["six"]);
            Text7.Text = Convert.ToString(Session["seven"]);
            Text8.Text = Convert.ToString(Session["eight"]);
            Text9.Text = Convert.ToString(Session["nine"]);
            Text10.Text = Convert.ToString(Session["ten"]);

            string[] radioval2 = s2.Split(new char[] { ',' });
            Session["one2"] = radioval2[0];
            Session["two2"] = radioval2[1];
            Session["three2"] = radioval2[2];
            Session["four2"] = radioval2[3];
            Session["five2"] = radioval2[4];
            Session["six2"] = radioval2[5];
            Session["seven2"] = radioval2[6];
            Session["eight2"] = radioval2[7];
            Session["nine2"] = radioval2[8];
            Session["ten2"] = radioval2[9];
            Text11.Text = Convert.ToString(Session["one2"]);
            Text12.Text = Convert.ToString(Session["two2"]);
            Text13.Text = Convert.ToString(Session["three2"]);
            Text14.Text = Convert.ToString(Session["four2"]);
            Text15.Text = Convert.ToString(Session["five2"]);
            Text16.Text = Convert.ToString(Session["six2"]);
            Text17.Text = Convert.ToString(Session["seven2"]);
            Text18.Text = Convert.ToString(Session["eight2"]);
            Text19.Text = Convert.ToString(Session["nine2"]);
            Text20.Text = Convert.ToString(Session["ten2"]);

            string[] radioval3 = s3.Split(new char[] { ',' });
            Session["one3"] = radioval3[0];
            Session["two3"] = radioval3[1];
            Session["three3"] = radioval3[2];
            Session["four3"] = radioval3[3];
            Session["five3"] = radioval3[4];
            Session["six3"] = radioval3[5];
            Session["seven3"] = radioval3[6];
            Session["eight3"] = radioval3[7];
            Session["nine3"] = radioval3[8];
            Session["ten3"] = radioval3[9];
            if (Convert.ToString(Session["one3"]) == "I")
            {
                Text21.Text = "25";
            }
            else
            {
                Text21.Text = "100";
            }
            if (Convert.ToString(Session["two3"]) == "I")
            {
                Text22.Text = "25";
            }
            else
            {
                Text22.Text = "100";
            }
            if (Convert.ToString(Session["three3"]) == "I")
            {
                Text23.Text = "25";
            }
            else
            {
                Text23.Text = "100";
            }
            if (Convert.ToString(Session["four3"]) == "I")
            {
                Text24.Text = "25";
            }
            else
            {
                Text24.Text = "100";
            }
            if (Convert.ToString(Session["five3"]) == "I")
            {
                Text25.Text = "25";
            }
            else
            {
                Text25.Text = "100";
            }
            if (Convert.ToString(Session["six3"]) == "I")
            {
                Text26.Text = "25";
            }
            else
            {
                Text26.Text = "100";
            }
            if (Convert.ToString(Session["seven3"]) == "I")
            {
                Text27.Text = "25";
            }
            else
            {
                Text27.Text = "100";
            }
            if (Convert.ToString(Session["eight3"]) == "I")
            {
                Text28.Text = "25";
            }
            else
            {
                Text28.Text = "100";
            }
            if (Convert.ToString(Session["nine3"]) == "I")
            {
                Text29.Text = "25";
            }
            else
            {
                Text29.Text = "100";
            }
            if (Convert.ToString(Session["ten3"]) == "I")
            {
                Text30.Text = "25";
            }
            else
            {
                Text30.Text = "100";
            }
            
        }
        Button3.Visible = true;
        select.Visible = false;
        
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("AdderLook.aspx");
    }
}
