
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_release_order : System.Web.UI.Page
{
    string checkpost = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ReleaseOrd_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {                
                if (Convert.ToString(Session["release_ord_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["release_ord_add_button"] = null;
                }
                if (Session["view_rel_order_reckey_rec_index"] != null)
                {
                    try
                    {
                        //GridView1.SelectedIndex = Convert.ToInt32(Session["view_rel_order_reckey_rec_index"]);
                        GridView1.SelectedIndex = 0;
                        Session["view_rel_order_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_rel_order_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].FormView2_actnumTextBox.value=e[1];");
        str.Append("var line = document.getElementById('FormView2_arlineLabel');");
        str.Append("line.value = e[0];");
        str.Append("document.forms[0].FormView2_inv_qtyTextBox.value=e[2];");
        str.Append("var tmsf2 = document.getElementById('FormView2_totl_msfLabel');");
        str.Append("tmsf2.value = e[3];");
        str.Append("var ino = document.getElementById('FormView2_i_noLabel');");
        str.Append("ino.value = e[4];");
        str.Append("var sn = document.getElementById('FormView2_snumLabel');");
        str.Append("sn.value = e[5];");
        str.Append("var accdesc = document.getElementById('FormView2_actdscrlabel');");
        str.Append("accdesc.value = e[6];}");

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }
       
       


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox cust = (TextBox)FormView1.FindControl("custLabel");
           TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
           Label status = (Label)FormView1.FindControl("statusLabel");
           Label printed = (Label)FormView1.FindControl("printedLabel");
           Label custadd1 = (Label)FormView1.FindControl("custadd1Label");
           Label shipadd1 = (Label)FormView1.FindControl("shipadd1Label");
           Label rellno = (Label)FormView1.FindControl("rellLabel");
           Label custadd2 = (Label)FormView1.FindControl("custadd2Label");
           Label shipadd2 = (Label)FormView1.FindControl("shipadd2Label");
           TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
           TextBox reldate = (TextBox)FormView1.FindControl("relldateTextBox");
           Label custcty = (Label)FormView1.FindControl("custctyLabel");
           Label custstat = (Label)FormView1.FindControl("custstatLabel");
           Label custzip = (Label)FormView1.FindControl("custzipLabel");
           Label shipcty = (Label)FormView1.FindControl("shipctyLabel");
           Label shipstat = (Label)FormView1.FindControl("shipstatLabel");
           Label shipzip = (Label)FormView1.FindControl("shipzipLabel");
           Label dtchang = (Label)FormView1.FindControl("dtchngLabel");
           Label usr = (Label)FormView1.FindControl("usrLabel");
           Label fgitm = (Label)FormView1.FindControl("fgitmLabel");
           TextBox trailer = (TextBox)FormView1.FindControl("trailerTextBox");
           Label ordqty = (Label)FormView1.FindControl("qtyordLabel");
           Label qtyrell = (Label)FormView1.FindControl("qtyrellLabel");
           Label qtyship = (Label)FormView1.FindControl("qtyshipLabel");
           Label qtyhand = (Label)FormView1.FindControl("qtyhandLabel");
           TextBox rno = (TextBox)FormView1.FindControl("extraTextBox");
           
           cust.Focus();
           ordqty.Text = "0";
           qtyrell.Text = "0";
           qtyship.Text = "0";
           qtyhand.Text = "0";
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               release con = new release();
               DataSet ds = new DataSet();
               ds = con.ReleaseOrderlist("CreateAdd", "", UserLogin.UserName, 0, 0, "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", "", "");

               reldate.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
               rno.Text = Convert.ToString(ds.Tables[0].Rows[0][40]);
               rellno.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               /*carrier.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
               reldate.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
               trailer.Text = Convert.ToString(ds.Tables[0].Rows[0][16]);*/
               usr.Text = UserLogin.UserName;               
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;  

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
           shipid.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {           
           try
           {
           Button AddButton = (Button)FormView1.FindControl("AddButton");
           Button UpdateButton = (Button)FormView1.FindControl("UpdateButton");
           Button DeleteButton = (Button)FormView1.FindControl("DeleteButton");


           Label rno = (Label)FormView1.FindControl("extraTextBox");
           Label reckey = (Label)FormView1.FindControl("reckeyTextBox");
           Label post = (Label)FormView1.FindControl("postedLabel");           

           Session["Release_ord_reckey_rec"] = reckey.Text.Trim();
           Session["Release_ord_r_no"] = rno.Text.Trim();
           if (post.Text == "yes")
           {
               checkpost = "Yes";
               AddButton.Enabled = false;
               UpdateButton.Enabled = false;
               DeleteButton.Enabled = false;
           }
               
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyTextBox");
       Label extra = (Label)FormView1.FindControl("extraTextBox");
       UserClass UserLogin = (UserClass)Session["User"];

       release con = new release();
       try
       {
           bool check = con.ValidateRelOrdlist("DeleteValidate", "", UserLogin.UserName, 0, 0, "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", reckey.Text, extra.Text.Trim());

           string value = Convert.ToString(check);
           if (value == "True")
           {

               ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
               ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
               ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
               ObjectDataSource1.SelectParameters["prmextra"].DefaultValue = extra.Text.Trim();

               FormView1.ChangeMode(FormViewMode.ReadOnly);
               Response.Write("<script>window.location.href='view_release_order.aspx'</script>");
           }
       }
       catch { }
       
   }
   protected void InsertCancelButton_Click(object sender, EventArgs e)
   {
       TextBox rno = (TextBox)FormView1.FindControl("extraTextBox");
       UserClass UserLogin = (UserClass)Session["User"];
       
       try
       {
           release con = new release();
           bool check = con.ValidateRelOrdlist("DeleteValidate", "", UserLogin.UserName, 0, 0, "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", "", rno.Text.Trim());

           string value = Convert.ToString(check);
           if (value == "True")
           {
               release con2 = new release();
               DataSet ds = new DataSet();
               ds = con2.ReleaseOrderlist("Delete", "", UserLogin.UserName, 0, 0, "", "", "", "", "", "", "", 0, "", "", "", "", "", "", "", "", rno.Text.Trim());
           }
       }
       catch { }

   }
   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       Label cust = (Label)FormView1.FindControl("custLabel");
       TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
       Label status = (Label)FormView1.FindControl("statusLabel");
       Label printed = (Label)FormView1.FindControl("printedLabel");
       Label custadd1 = (Label)FormView1.FindControl("custadd1Label");
       Label shipadd1 = (Label)FormView1.FindControl("shipadd1Label");
       Label rellno = (Label)FormView1.FindControl("rellLabel");
       Label custadd2 = (Label)FormView1.FindControl("custadd2Label");
       Label shipadd2 = (Label)FormView1.FindControl("shipadd2Label");
       TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
       TextBox reldate = (TextBox)FormView1.FindControl("relldateTextBox");
       Label custcty = (Label)FormView1.FindControl("custctyLabel");
       Label custstat = (Label)FormView1.FindControl("custstatLabel");
       Label custzip = (Label)FormView1.FindControl("custzipLabel");
       Label shipcty = (Label)FormView1.FindControl("shipctyLabel");
       Label shipstat = (Label)FormView1.FindControl("shipstatLabel");
       Label shipzip = (Label)FormView1.FindControl("shipzipLabel");
       Label dtchang = (Label)FormView1.FindControl("dtchngLabel");
       Label usr = (Label)FormView1.FindControl("usrLabel");
       Label fgitm = (Label)FormView1.FindControl("fgitmLabel");
       TextBox trailer = (TextBox)FormView1.FindControl("trailerTextBox");
       Label ordqty = (Label)FormView1.FindControl("qtyordLabel");
       Label qtyrell = (Label)FormView1.FindControl("qtyrellLabel");
       Label qtyship = (Label)FormView1.FindControl("qtyshipLabel");
       Label qtyhand = (Label)FormView1.FindControl("qtyhandLabel");

       UserClass UserLogin = (UserClass)Session["User"];
       
       try
       {
           release con = new release();
           bool check = con.ValidateRelOrdlist("ValidateUpdate", "", UserLogin.UserName, 0, 0, "", cust.Text.Trim(), "", shipid.Text.Trim(), "", "", "", 0, "", "", "", "", "", "", "", "", Convert.ToString(Session["Release_ord_r_no"]));

           string value = Convert.ToString(check);
           if (value == "True")
           {

               ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
               ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
               ObjectDataSource1.SelectParameters["prmshipid"].DefaultValue = shipid.Text.Trim();
               ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
               ObjectDataSource1.SelectParameters["prmreldate"].DefaultValue = reldate.Text.Trim();
               ObjectDataSource1.SelectParameters["prmtrailer"].DefaultValue = trailer.Text.Trim();

               FormView1.ChangeMode(FormViewMode.ReadOnly);
           }
       }
       catch { }
   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox cust = (TextBox)FormView1.FindControl("custLabel");
       TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
       Label status = (Label)FormView1.FindControl("statusLabel");
       Label printed = (Label)FormView1.FindControl("printedLabel");
       Label custadd1 = (Label)FormView1.FindControl("custadd1Label");
       Label shipadd1 = (Label)FormView1.FindControl("shipadd1Label");
       Label rellno = (Label)FormView1.FindControl("rellLabel");
       Label custadd2 = (Label)FormView1.FindControl("custadd2Label");
       Label shipadd2 = (Label)FormView1.FindControl("shipadd2Label");
       TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
       TextBox reldate = (TextBox)FormView1.FindControl("relldateTextBox");
       Label custcty = (Label)FormView1.FindControl("custctyLabel");
       Label custstat = (Label)FormView1.FindControl("custstatLabel");
       Label custzip = (Label)FormView1.FindControl("custzipLabel");
       Label shipcty = (Label)FormView1.FindControl("shipctyLabel");
       Label shipstat = (Label)FormView1.FindControl("shipstatLabel");
       Label shipzip = (Label)FormView1.FindControl("shipzipLabel");
       Label dtchang = (Label)FormView1.FindControl("dtchngLabel");
       Label usr = (Label)FormView1.FindControl("usrLabel");
       Label fgitm = (Label)FormView1.FindControl("fgitmLabel");
       TextBox trailer = (TextBox)FormView1.FindControl("trailerTextBox");
       Label ordqty = (Label)FormView1.FindControl("qtyordLabel");
       Label qtyrell = (Label)FormView1.FindControl("qtyrellLabel");
       Label qtyship = (Label)FormView1.FindControl("qtyshipLabel");
       Label qtyhand = (Label)FormView1.FindControl("qtyhandLabel");
       TextBox rno = (TextBox)FormView1.FindControl("extraTextBox");

       UserClass UserLogin = (UserClass)Session["User"];
       
       try
       {
           release con = new release();
           bool check = con.ValidateRelOrdlist("ValidateAdd", "", UserLogin.UserName, 0, 0, "", cust.Text.Trim(), "", shipid.Text.Trim(), "", "", "", 0, "", "", "", "", "", "", "", "", "");

           string value = Convert.ToString(check);
           if (value == "True")
           {
               Session["Release_ord_r_no"] = rno.Text.Trim();
               ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
               ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";

               ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = cust.Text.Trim();
               ObjectDataSource1.SelectParameters["prmshipid"].DefaultValue = shipid.Text.Trim();
               ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
               ObjectDataSource1.SelectParameters["prmreldate"].DefaultValue = reldate.Text.Trim();
               ObjectDataSource1.SelectParameters["prmtrailer"].DefaultValue = trailer.Text.Trim();

               FormView1.ChangeMode(FormViewMode.ReadOnly);
               FormView2.ChangeMode(FormViewMode.Insert);
           }
       }
       catch { }
   }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_release_order.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("ReleaseOrd_list.aspx");
    }

    protected void custext_Change(object sender, EventArgs e)
    {
        TextBox cust = (TextBox)FormView1.FindControl("custLabel");
        TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
        Label status = (Label)FormView1.FindControl("statusLabel");
        Label printed = (Label)FormView1.FindControl("printedLabel");
        Label custadd1 = (Label)FormView1.FindControl("custadd1Label");
        Label shipadd1 = (Label)FormView1.FindControl("shipadd1Label");
        Label rellno = (Label)FormView1.FindControl("rellLabel");
        Label custadd2 = (Label)FormView1.FindControl("custadd2Label");
        Label shipadd2 = (Label)FormView1.FindControl("shipadd2Label");
        TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
        TextBox reldate = (TextBox)FormView1.FindControl("relldateTextBox");
        Label custcty = (Label)FormView1.FindControl("custctyLabel");
        Label custstat = (Label)FormView1.FindControl("custstatLabel");
        Label custzip = (Label)FormView1.FindControl("custzipLabel");
        Label shipcty = (Label)FormView1.FindControl("shipctyLabel");
        Label shipstat = (Label)FormView1.FindControl("shipstatLabel");
        Label shipzip = (Label)FormView1.FindControl("shipzipLabel");
        Label dtchang = (Label)FormView1.FindControl("dtchngLabel");
        Label usr = (Label)FormView1.FindControl("usrLabel");
        Label fgitm = (Label)FormView1.FindControl("fgitmLabel");
        TextBox trailer = (TextBox)FormView1.FindControl("trailerTextBox");
        Label ordqty = (Label)FormView1.FindControl("qtyordLabel");
        Label qtyrell = (Label)FormView1.FindControl("qtyrellLabel");
        Label qtyship = (Label)FormView1.FindControl("qtyshipLabel");
        Label qtyhand = (Label)FormView1.FindControl("qtyhandLabel");
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            release con = new release();
            DataSet ds = new DataSet();

            ds = con.ReleaseOrderlist("View", "", UserLogin.UserName, 0, 0, "", cust.Text.Trim(), "", "", "", "", "", 0, "", "", "", "", "", "", "", "", "");

            custadd1.Text = Convert.ToString(ds.Tables[0].Rows[0][19]);
            custadd2.Text = Convert.ToString(ds.Tables[0].Rows[0][20]);
            custcty.Text = Convert.ToString(ds.Tables[0].Rows[0][21]);
            custstat.Text = Convert.ToString(ds.Tables[0].Rows[0][22]);
            custzip.Text = Convert.ToString(ds.Tables[0].Rows[0][23]);            
            cust.Focus();
        }
        catch { }

    }

    protected void shiptext_Change(object sender, EventArgs e)
    {
        TextBox cust = (TextBox)FormView1.FindControl("custLabel");
        TextBox shipid = (TextBox)FormView1.FindControl("shiptoTextBox");
        Label status = (Label)FormView1.FindControl("statusLabel");
        Label printed = (Label)FormView1.FindControl("printedLabel");
        Label custadd1 = (Label)FormView1.FindControl("custadd1Label");
        Label shipadd1 = (Label)FormView1.FindControl("shipadd1Label");
        Label rellno = (Label)FormView1.FindControl("rellLabel");
        Label custadd2 = (Label)FormView1.FindControl("custadd2Label");
        Label shipadd2 = (Label)FormView1.FindControl("shipadd2Label");
        TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
        TextBox reldate = (TextBox)FormView1.FindControl("relldateTextBox");
        Label custcty = (Label)FormView1.FindControl("custctyLabel");
        Label custstat = (Label)FormView1.FindControl("custstatLabel");
        Label custzip = (Label)FormView1.FindControl("custzipLabel");
        Label shipcty = (Label)FormView1.FindControl("shipctyLabel");
        Label shipstat = (Label)FormView1.FindControl("shipstatLabel");
        Label shipzip = (Label)FormView1.FindControl("shipzipLabel");
        Label dtchang = (Label)FormView1.FindControl("dtchngLabel");
        Label usr = (Label)FormView1.FindControl("usrLabel");
        Label fgitm = (Label)FormView1.FindControl("fgitmLabel");
        TextBox trailer = (TextBox)FormView1.FindControl("trailerTextBox");
        Label ordqty = (Label)FormView1.FindControl("qtyordLabel");
        Label qtyrell = (Label)FormView1.FindControl("qtyrellLabel");
        Label qtyship = (Label)FormView1.FindControl("qtyshipLabel");
        Label qtyhand = (Label)FormView1.FindControl("qtyhandLabel");
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            release con = new release();
            DataSet ds = new DataSet();

            ds = con.ReleaseOrderlist("View", "", UserLogin.UserName, 0, 0, "", cust.Text.Trim(), "", shipid.Text.Trim(), "", "", "", 0, "", "", "", "", "", "", "", "", "");

            shipadd1.Text = Convert.ToString(ds.Tables[0].Rows[0][25]);
            shipadd2.Text = Convert.ToString(ds.Tables[0].Rows[0][26]);
            shipcty.Text = Convert.ToString(ds.Tables[0].Rows[0][27]);
            shipstat.Text = Convert.ToString(ds.Tables[0].Rows[0][28]);
            shipzip.Text = Convert.ToString(ds.Tables[0].Rows[0][29]);
            carrier.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            shipid.Focus();
        }
        catch { }

    }

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_rel_order_reckey_rec_index"] = GridView1.SelectedIndex;
        Session["view_rel_order_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        Label ordno = (Label)FormView2.FindControl("ordnoTextBox");
        TextBox ino = (TextBox)FormView2.FindControl("fgitemTextBox");
        TextBox custpo = (TextBox)FormView2.FindControl("custpoTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
        TextBox tag = (TextBox)FormView2.FindControl("tagTextBox");
        TextBox whse = (TextBox)FormView2.FindControl("whseTextBox");
        TextBox bin = (TextBox)FormView2.FindControl("binlocTextBox");
        TextBox job = (TextBox)FormView2.FindControl("jobnoTextBox");
        TextBox job2 = (TextBox)FormView2.FindControl("jobno2TextBox");
        TextBox custno = (TextBox)FormView2.FindControl("custnoTextBox");
        TextBox unit = (TextBox)FormView2.FindControl("unitTextBox");
        TextBox qtyunit = (TextBox)FormView2.FindControl("qtyuntTextBox");
        TextBox partial = (TextBox)FormView2.FindControl("partialTextBox");
        Label relno = (Label)FormView2.FindControl("relnoLabel");
        Label board = (Label)FormView2.FindControl("boardLabel");
        TextBox scod = (TextBox)FormView2.FindControl("scodTextBox");
        TextBox custpart = (TextBox)FormView2.FindControl("custpartTextBox");
        TextBox seq = (TextBox)FormView2.FindControl("relseqTextBox");

        if (qty.Text == "")
            qty.Text = "0";
        if (unit.Text == "")
            unit.Text = "0";
        if (qtyunit.Text == "")
            qtyunit.Text = "0";
        if (partial.Text == "")
            partial.Text = "0";

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");       
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            release con = new release();

            bool check = con.ValidateViewRelOrd("ValidateUpdate", "", UserLogin.UserName, Convert.ToInt32(ordno.Text.Trim()), ino.Text.Trim(), "", 0, tag.Text.Trim(), whse.Text.Trim(), bin.Text.Trim(), job.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), custno.Text.Trim(), 0, 0, 0, Convert.ToInt32(relno.Text.Trim()), 0, scod.Text.Trim(), "", 0, reckey.Text.Trim(), Convert.ToString(Session["Release_ord_r_no"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmordno2"].DefaultValue = ordno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmino2"].DefaultValue = ino.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmpono2"].DefaultValue = custpo.Text.Trim();
                ObjectDataSource3.SelectParameters["prmqty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmtag"].DefaultValue = tag.Text.Trim();
                ObjectDataSource3.SelectParameters["prmloc"].DefaultValue = whse.Text.Trim();
                ObjectDataSource3.SelectParameters["prmlocbin"].DefaultValue = bin.Text.Trim();
                ObjectDataSource3.SelectParameters["prmjobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource3.SelectParameters["prmjobno2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcustno"].DefaultValue = custno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcases"].DefaultValue = unit.Text.Trim();
                ObjectDataSource3.SelectParameters["prmqtycas"].DefaultValue = qtyunit.Text.Trim();
                ObjectDataSource3.SelectParameters["prmpartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource3.SelectParameters["prmrelno2"].DefaultValue = relno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbordno"].DefaultValue = board.Text.Trim();
                ObjectDataSource3.SelectParameters["prmscod"].DefaultValue = scod.Text.Trim();
                ObjectDataSource3.SelectParameters["prmpartno"].DefaultValue = custpart.Text.Trim();
                ObjectDataSource3.SelectParameters["prmlinkno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource3.SelectParameters["prmreckey"].DefaultValue = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmextra2"].DefaultValue = Convert.ToString(Session["Release_ord_r_no"]);

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_release_order.aspx'</script>");
            }
        }
        catch { }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox ordno = (TextBox)FormView2.FindControl("ordnoTextBox");
        TextBox ino = (TextBox)FormView2.FindControl("fgitemTextBox");
        TextBox custpo = (TextBox)FormView2.FindControl("custpoTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
        TextBox tag = (TextBox)FormView2.FindControl("tagTextBox");
        TextBox whse = (TextBox)FormView2.FindControl("whseTextBox");
        TextBox bin = (TextBox)FormView2.FindControl("binlocTextBox");
        TextBox job = (TextBox)FormView2.FindControl("jobnoTextBox");
        TextBox job2 = (TextBox)FormView2.FindControl("jobno2TextBox");
        TextBox custno = (TextBox)FormView2.FindControl("custnoTextBox");
        TextBox unit = (TextBox)FormView2.FindControl("unitTextBox");
        TextBox qtyunit = (TextBox)FormView2.FindControl("qtyuntTextBox");
        TextBox partial = (TextBox)FormView2.FindControl("partialTextBox");
        Label relno = (Label)FormView2.FindControl("relnoLabel");
        Label board = (Label)FormView2.FindControl("boardLabel");
        TextBox scod = (TextBox)FormView2.FindControl("scodTextBox");
        TextBox custpart = (TextBox)FormView2.FindControl("custpartTextBox");
        TextBox seq = (TextBox)FormView2.FindControl("relseqTextBox");


        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        if (qty.Text == "")
            qty.Text = "0";
        if (unit.Text == "")
            unit.Text = "0";
        if (qtyunit.Text == "")
            qtyunit.Text = "0";
        if (partial.Text == "")
            partial.Text = "0";
        if (relno.Text == "")
            relno.Text = "0";

        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            release con = new release();

            bool check = con.ValidateViewRelOrd("ValidateAdd", "", UserLogin.UserName, Convert.ToInt32(ordno.Text.Trim()), ino.Text.Trim(), "", 0, tag.Text.Trim(), whse.Text.Trim(), bin.Text.Trim(), job.Text.Trim(), Convert.ToInt32(job2.Text.Trim()), custno.Text.Trim(), 0, 0, 0, Convert.ToInt32(relno.Text.Trim()), 0, scod.Text.Trim(), "", 0, reckey.Text.Trim(), Convert.ToString(Session["Release_ord_r_no"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["view_rel_order_reckey_rec"] = reckey.Text.Trim();

                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmordno2"].DefaultValue = ordno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmino2"].DefaultValue = ino.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmpono2"].DefaultValue = custpo.Text.Trim();
                ObjectDataSource3.SelectParameters["prmqty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmtag"].DefaultValue = tag.Text.Trim();
                ObjectDataSource3.SelectParameters["prmloc"].DefaultValue = whse.Text.Trim();
                ObjectDataSource3.SelectParameters["prmlocbin"].DefaultValue = bin.Text.Trim();
                ObjectDataSource3.SelectParameters["prmjobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource3.SelectParameters["prmjobno2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcustno"].DefaultValue = custno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcases"].DefaultValue = unit.Text.Trim();
                ObjectDataSource3.SelectParameters["prmqtycas"].DefaultValue = qtyunit.Text.Trim();
                ObjectDataSource3.SelectParameters["prmpartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource3.SelectParameters["prmrelno2"].DefaultValue = relno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbordno"].DefaultValue = board.Text.Trim();
                ObjectDataSource3.SelectParameters["prmscod"].DefaultValue = scod.Text.Trim();
                ObjectDataSource3.SelectParameters["prmpartno"].DefaultValue = custpart.Text.Trim();
                ObjectDataSource3.SelectParameters["prmlinkno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource3.SelectParameters["prmreckey"].DefaultValue = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmextra2"].DefaultValue = Convert.ToString(Session["Release_ord_r_no"]);

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_release_order.aspx'</script>");
            }
        }
        catch { }       

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox ordno = (TextBox)FormView2.FindControl("ordnoTextBox");
            TextBox ino = (TextBox)FormView2.FindControl("fgitemTextBox");
            TextBox custpo = (TextBox)FormView2.FindControl("custpoTextBox");
            TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
            TextBox tag = (TextBox)FormView2.FindControl("tagTextBox");
            TextBox whse = (TextBox)FormView2.FindControl("whseTextBox");
            TextBox bin = (TextBox)FormView2.FindControl("binlocTextBox");
            TextBox job = (TextBox)FormView2.FindControl("jobnoTextBox");
            TextBox job2 = (TextBox)FormView2.FindControl("jobno2TextBox");
            TextBox custno = (TextBox)FormView2.FindControl("custnoTextBox");
            TextBox unit = (TextBox)FormView2.FindControl("unitTextBox");
            TextBox qtyunit = (TextBox)FormView2.FindControl("qtyuntTextBox");
            TextBox partial = (TextBox)FormView2.FindControl("partialTextBox");
            Label relno = (Label)FormView2.FindControl("relnoLabel");
            Label board = (Label)FormView2.FindControl("boardLabel");
            TextBox scod = (TextBox)FormView2.FindControl("scodTextBox");
            TextBox custpart = (TextBox)FormView2.FindControl("custpartTextBox");
            TextBox seq = (TextBox)FormView2.FindControl("relseqTextBox");
            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            TextBox extra = (TextBox)FormView2.FindControl("extraTextBox");

            ordno.Focus();
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                
                    release con = new release();
                    DataSet ds = new DataSet();
                    ds = con.ViewReleaseOrder("CreateAdd", "", UserLogin.UserName, 0, "", "", 0, "", "", "", "", 0, "", 0, 0, 0, 0, 0, "", "", 0, "", Convert.ToString(Session["Release_ord_r_no"]));

                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][19]);
                                        
                    
                    //GridView1.Visible = false;
                
                
            }
            catch { }

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            
            /*actnum.Focus();
            GridView1.Visible = false;*/
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Button AddButton = (Button)FormView2.FindControl("AddButton");
                Button UpdateItemButton = (Button)FormView2.FindControl("UpdateItemButton");
                Button DeleteButton = (Button)FormView2.FindControl("DeleteButton");
                   Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                   Session["view_rel_order_reckey_rec"] = reckey.Text.Trim();
                   Session["Release_ord_reckey_rec"] = reckey.Text.Trim();
                   if (checkpost == "Yes")
                   {
                       AddButton.Enabled = false;
                       UpdateItemButton.Enabled = false;
                       DeleteButton.Enabled = false;
                   }

            }
            catch { }
            GridView1.Visible = true;
            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            release con = new release();
            DataSet ds = new DataSet();
            ds = con.ViewReleaseOrder("DataDelete", "", UserLogin.UserName, 0, "", "", 0, "", "", "", "", 0, "", 0, 0, 0, 0, 0, "", "", 0, reckey.Text.Trim(), Convert.ToString(Session["Release_ord_r_no"]));
        }
        catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_release_order.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
        try
        {
            release con = new release();

            bool check = con.ValidateViewRelOrd("DeleteValidate", "", UserLogin.UserName, 0, "", "", 0, "", "", "", "", 0, "", 0, 0, 0, 0, 0, "", "", 0, reckey.Text.Trim(), Convert.ToString(Session["Release_ord_r_no"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                Response.Write("<script>window.location.href='view_release_order.aspx'</script>");
            }
        }
        catch { }            

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
    }

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }

    

    protected void load_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("rel_ord_shipnote.aspx");
    }

}
