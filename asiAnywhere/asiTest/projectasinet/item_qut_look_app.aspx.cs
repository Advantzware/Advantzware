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

public partial class item_qut_look_app : System.Web.UI.Page
{
    int tot = 0;
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["PrmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            Button3.Visible = false;
            //ddlSearchOperation.SelectedIndex = 1;
            
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

        select.Attributes.Add("onclick", "Tovalue();");
        

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = "Quote" ;
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = "EQUAL";
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sea";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
       
       //Response.Write(Session["genestno"]);
        //GridView1.Visible = false;
        //GridView2.Visible = true;
        //tblSearch.Visible = false;
        
        
    }

    protected void select_click(object sender, EventArgs e)
    {
        StringBuilder str1 = new StringBuilder();
        StringBuilder str2 = new StringBuilder();
        StringBuilder str3 = new StringBuilder();
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool isChecked = ((CheckBox)row.FindControl("chkSelect")).Checked;

            if (isChecked)
            {
                tot = tot + 1;
                str1.Append(GridView1.Rows[i].Cells[1].Text);
                str1.Append(",");
                str2.Append(GridView1.Rows[i].Cells[2].Text);
                str2.Append(",");
                str3.Append(GridView1.Rows[i].Cells[3].Text);
                str3.Append(",");
            }
            
        }
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool isChecked = ((CheckBox)row.FindControl("chkSelect")).Checked;

            if (isChecked)
            {
                Text11.Text = GridView1.Rows[i].Cells[2].Text;
                Text12.Text = GridView1.Rows[i].Cells[3].Text.Trim();
                Text13.Text = ((Label)GridView1.Rows[i].FindControl("viname2Label")).Text;
                Text14.Text = ((Label)GridView1.Rows[i].FindControl("vhandLabel")).Text;
                Text15.Text = ((Label)GridView1.Rows[i].FindControl("vcustLabel")).Text;
                Text16.Text = GridView1.Rows[i].Cells[4].Text.Trim(); // custpart
                Text17.Text = GridView1.Rows[i].Cells[5].Text.Trim(); // vdesc
                Text18.Text = ((Label)GridView1.Rows[i].FindControl("vestLabel")).Text;
                Text19.Text = GridView1.Rows[i].Cells[6].Text.Trim(); //part desc
                Text20.Text = GridView1.Rows[i].Cells[7].Text.Trim();  // partdesc
                Text21.Text = GridView1.Rows[i].Cells[8].Text.Trim();  // vprice
                Text22.Text = GridView1.Rows[i].Cells[9].Text.Trim();  // vuom
                Text23.Text = ((Label)GridView1.Rows[i].FindControl("vtypeLabel")).Text;
                Text24.Text = GridView1.Rows[i].Cells[10].Text.Trim(); // discount
                Text25.Text = GridView1.Rows[i].Cells[11].Text.Trim(); // qty
                Text26.Text = ((Label)GridView1.Rows[i].FindControl("vqtyunitLabel")).Text;
                Text27.Text = ((Label)GridView1.Rows[i].FindControl("vunitpalletLabel")).Text;
                Text28.Text = ((Label)GridView1.Rows[i].FindControl("vpartialLabel")).Text;
                Text29.Text = ((Label)GridView1.Rows[i].FindControl("qtymultiLabel")).Text;
               
                break;
            }
            
                
            
        }

        string ss = str1.ToString();
        string s2 = str2.ToString();
        string s3 = str3.ToString();


        if (tot == 1)
        {
            string[] radioval = s3.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            
            
            Text1.Text = Convert.ToString(Session["one"]);
            //Text2.Text = Convert.ToString(Session["two"]);
            //Text3.Text = Convert.ToString(Session["three"]);
            //Text4.Text = Convert.ToString(Session["four"]);
            //Text5.Text = Convert.ToString(Session["five"]);
            //Text6.Text = Convert.ToString(Session["six"]);
            //Text7.Text = Convert.ToString(Session["seven"]);
            //Text8.Text = Convert.ToString(Session["eight"]);
            //Text9.Text = Convert.ToString(Session["nine"]);
            //Text10.Text = "";
        }
        if (tot == 2)
        {
            string[] radioval = s3.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];    
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            //Text3.Text = Convert.ToString(Session["three"]);
            //Text4.Text = Convert.ToString(Session["four"]);
            //Text5.Text = Convert.ToString(Session["five"]);
            //Text6.Text = Convert.ToString(Session["six"]);
            //Text7.Text = Convert.ToString(Session["seven"]);
            //Text8.Text = Convert.ToString(Session["eight"]);
            //Text9.Text = Convert.ToString(Session["nine"]);
            //Text10.Text = "";
        }
        
        if (tot == 3)
        {
            string[] radioval = s3.Split(new char[] { ',' });
            Session["one"] = radioval[0];
            Session["two"] = radioval[1];
            Session["three"] = radioval[2];
            
            Text1.Text = Convert.ToString(Session["one"]);
            Text2.Text = Convert.ToString(Session["two"]);
            Text3.Text = Convert.ToString(Session["three"]);
            //Text4.Text = Convert.ToString(Session["four"]);
            //Text5.Text = Convert.ToString(Session["five"]);
            //Text6.Text = Convert.ToString(Session["six"]);
            //Text7.Text = Convert.ToString(Session["seven"]);
            //Text8.Text = Convert.ToString(Session["eight"]);
            //Text9.Text = Convert.ToString(Session["nine"]);
            //Text10.Text = "";
        }


            
        
        
        //Button3.Visible = true;
        //select.Visible = false;

        

    }

    protected void dbGrid1_main_menu_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        //string myColor = "";
        //DataRowView drv = (DataRowView)e.Row.DataItem;
        //if (e.Row.RowType == DataControlRowType.DataRow)
        //{
        //    myColor = DataBinder.Eval(e.Row.DataItem, "ID").ToString();
        //}
                
        //LinkButton total = (LinkButton)e.Row.FindControl("totalButton");

        //if (ds.Tables[0].Rows.Count != 0)
        //    for (int i = 0; i < 15; i++)
        //    {
        //        if (ds.Tables[0].Rows[0][i + 5].ToString() != "")
        //            GridView1.HeaderRow.Cells[i + 6].Text = ds.Tables[0].Rows[0][i + 5].ToString();

        //    }
        //try
        //{
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label sub1 = (Label)e.Row.FindControl("qtymultiLabel");
        try
        {
            LookUp lookqty = new LookUp();
            DataSet dsqty = new DataSet();
            //int qty = Convert.ToInt32( e.Row.Cells(6).text);
            string quote1 = Convert.ToString(DataBinder.Eval(e.Row.DataItem, "vqno"));
            string fgitem = Convert.ToString(DataBinder.Eval(e.Row.DataItem, "vino2"));
            string custpart = Convert.ToString(DataBinder.Eval(e.Row.DataItem, "vcustpart2"));
            
            if (quote1 == "")
                quote1 = "0";
            dsqty = lookqty.ItemQuantityLook(UserLogin.UserName, Convert.ToInt32(quote1), fgitem, custpart);
            if (dsqty.Tables[0].Rows.Count > 1)
            {                                
                sub1.Text = "multi";
            }
            else
            {                
                sub1.Text = "only";
            }
        }
        catch { }



    }


}
