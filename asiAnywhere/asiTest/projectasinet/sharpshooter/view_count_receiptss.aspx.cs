using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for vieworder
/// </summary>

namespace projectasinet.sharpshooter
{
    public partial class view_count_receiptss : System.Web.UI.Page
    {

        protected void Page_Load(object sender, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
            if (Convert.ToString(Session["count_receipt_list_add_new"]) == "Yes")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                Session["count_receipt_list_add_new"] = null;
            }

            if (!Page.IsPostBack)
            {
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "count_receipt_list.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();
                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (vCanRun == false)
                    {
                        Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                        Response.Write("<script>window.location.href = 'login.aspx';</script>");

                    }
                    lblComp.Text = PrmComp;
                    lblUser.Text = UserLogin.UserName;


                }

            }

        }


        protected void LinkButton1_Click(object sender, EventArgs e)
        {
            Response.Redirect("menu.aspx");
        }
        protected void hlnkLogOut_Click(object sender, EventArgs e)
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFileSs"];
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

        protected void lnk_list_click(object sender, EventArgs e)
        {
            Response.Redirect("count_receipt_listss.aspx");
        }

        protected void lnk_view_click(object sender, EventArgs e)
        {
            Response.Redirect("view_count_receiptss.aspx");
        }

        protected void UpdateButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("Label1");

            TextBox tag = (TextBox)FormView1.FindControl("TextBox1");
            TextBox vdate = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("TextBox4");
            TextBox itemname = (TextBox)FormView1.FindControl("TextBox5");
            TextBox loc = (TextBox)FormView1.FindControl("TextBox2");
            TextBox locbin = (TextBox)FormView1.FindControl("TextBox3");
            TextBox cases = (TextBox)FormView1.FindControl("TextBox6");
            TextBox qtycas = (TextBox)FormView1.FindControl("TextBox7");
            TextBox casunit = (TextBox)FormView1.FindControl("TextBox8");
            TextBox partial = (TextBox)FormView1.FindControl("TextBox9");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");


            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno2"].DefaultValue = jobno2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmlocbin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTqty"].DefaultValue = t_qty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);



        }
        protected void InsertButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("Label1");
            TextBox tag = (TextBox)FormView1.FindControl("TextBox1");
            TextBox vdate = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("TextBox4");
            TextBox itemname = (TextBox)FormView1.FindControl("TextBox5");
            TextBox loc = (TextBox)FormView1.FindControl("TextBox2");
            TextBox locbin = (TextBox)FormView1.FindControl("TextBox3");
            TextBox cases = (TextBox)FormView1.FindControl("TextBox6");
            TextBox qtycas = (TextBox)FormView1.FindControl("TextBox7");
            TextBox casunit = (TextBox)FormView1.FindControl("TextBox8");
            TextBox partial = (TextBox)FormView1.FindControl("TextBox9");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");


            Session["count_recept_list_seq"] = seq.Text.Trim();

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
            ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
            ObjectDataSource1.SelectParameters["prmJobno2"].DefaultValue = jobno2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();
            ObjectDataSource1.SelectParameters["prmlocbin"].DefaultValue = locbin.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
            ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTqty"].DefaultValue = t_qty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }

        protected void DeleteButton_Click(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            Label seq = (Label)FormView1.FindControl("vRnoLabel");
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();

        }
        protected void FormView1_Unload(object sender, EventArgs e)
        {

            try
            {
                Label seq = (Label)FormView1.FindControl("vRnoLabel");


                Session["count_recept_list_seq"] = seq.Text.Trim();


            }
            catch { }
        }

        protected void InserCancelButton_Click(object sender, EventArgs e)
        {
            Label seq = (Label)FormView1.FindControl("Label1");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();


            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }





        protected void ItemTextBox_Change(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("Label1");
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("TextBox1");

            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("TextBox4");
            TextBox itemname = (TextBox)FormView1.FindControl("TextBox5");
            TextBox loc = (TextBox)FormView1.FindControl("TextBox2");
            TextBox locbin = (TextBox)FormView1.FindControl("TextBox3");
            TextBox cases = (TextBox)FormView1.FindControl("TextBox6");
            TextBox qtycas = (TextBox)FormView1.FindControl("TextBox7");
            TextBox casunit = (TextBox)FormView1.FindControl("TextBox8");
            TextBox partial = (TextBox)FormView1.FindControl("TextBox9");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            if (item.Text.Trim() == "")
            {
                item.Focus();
                return;
            }
            try
            {
                itemhistory fglook = new itemhistory();
                DataSet ds = new DataSet();

                ds = fglook.PoSelLookup("ItemSearch", UserLogin.UserName, "Item", "EQUAL", item.Text.Trim());

                if (ds.Tables[0].Rows.Count == 0)
                {

                    item.Focus();
                }
                else
                {
                    itemname.Text = ds.Tables[0].Rows[0][14].ToString();
                    if (qtycas.Text == "" || qtycas.Text == "0")
                        qtycas.Text = ds.Tables[0].Rows[0][12].ToString();

                    if (loc.Text == "")
                        loc.Text = ds.Tables[0].Rows[0][15].ToString();
                    if (locbin.Text == "")
                        locbin.Text = ds.Tables[0].Rows[0][16].ToString();



                    casunit.Text = "1";
                    if (cases.Text == "0")
                    {
                        //partial.Text = "0";
                        //t_qty.Text = "0";
                    }
                    item.Focus();
                }
            }
            catch { }

        }

        protected void FormView1_PreRender(object sender, EventArgs e)
        {
            //try
            //{
            //    Label sql = (Label)FormView1.FindControl("vRecLabel");
            //    Session["trans_recept_list_seq"] = sql.Text;

            //}
            //catch { }
        }

        protected void FormView1_DataBound(object sender, EventArgs e)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox tag = (TextBox)FormView1.FindControl("TextBox1");
                tag.Focus();

            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                Label seq = (Label)FormView1.FindControl("Label1");
                TextBox tag = (TextBox)FormView1.FindControl("TextBox1");
                tag.Focus();


                TextBox vdate = (TextBox)FormView1.FindControl("vDateTextBox");
                Label vtime = (Label)FormView1.FindControl("vTransTimeLabel");
                Label created = (Label)FormView1.FindControl("Label2");
                Label created2 = (Label)FormView1.FindControl("Label3");
                vdate.Text = DateTime.Now.ToShortDateString();
                vtime.Text = DateTime.Now.ToString("HH:MM");
                created.Text = UserLogin.UserName;
                created2.Text = UserLogin.UserName;

                itemhistory rcpt = new itemhistory();
                DataSet dsrfqs = new DataSet();

                dsrfqs = rcpt.selectcountrcpt(UserLogin.UserName, "Addnewrcpt", "", "", "", 0, "", "", "", "", "", "", 0, 0, 0, 0, 0, "", "", "");

                seq.Text = dsrfqs.Tables[0].Rows[0][0].ToString();


            }
        }

        protected void img_btn_add_click(object sender, EventArgs e)
        {
            FormView1.ChangeMode(FormViewMode.Insert);

        }

        protected void bolno_textchange(object sender, EventArgs e)
        {
            Label SeqNum = (Label)FormView1.FindControl("vSeqNumTextBox");
            TextBox VendorBolNum = (TextBox)FormView1.FindControl("vVendBolNoTextBox");
            TextBox UsgDate = (TextBox)FormView1.FindControl("vUsgDateTextBox");
            TextBox QtyUsed = (TextBox)FormView1.FindControl("vQtyUsedTextBox");
            TextBox CustPoNum = (TextBox)FormView1.FindControl("vCustPoNumTextBox");
            TextBox CustPoLineNum = (TextBox)FormView1.FindControl("vCustPoLineNumTextBox");
            TextBox CustPartNum = (TextBox)FormView1.FindControl("vCustPartNumTextBox");
            TextBox FgItmNum = (TextBox)FormView1.FindControl("vFgItmNumTextBox");
            TextBox CustVenCode = (TextBox)FormView1.FindControl("vCustVenCodeTextBox");
            TextBox CustPlantId = (TextBox)FormView1.FindControl("vCustPlantIdTextBox");
            TextBox CustDptCod = (TextBox)FormView1.FindControl("vCustDptCodTextBox");
            TextBox VenOrdNum = (TextBox)FormView1.FindControl("vVenOrdNumTextBox");
            TextBox VenJobNum = (TextBox)FormView1.FindControl("vVenJobNumTextBox");
            TextBox VenJob2Num = (TextBox)FormView1.FindControl("vVenJob2NumTextBox");
            TextBox ItmSelPrice = (TextBox)FormView1.FindControl("vItmSelPriceTextBox");
            TextBox CustHandQty = (TextBox)FormView1.FindControl("vCustHandQtyTextBox");
            Label error = (Label)FormView1.FindControl("Errorlabel");
            try
            {
                LookUp look = new LookUp();
                DataSet ds = new DataSet();
                ds = look.VendorBolLook(Convert.ToString(Session["User"]), "search", "BolNum", "EQUAL", VendorBolNum.Text.Trim());
                // Response.Write(ds.Tables[0].Rows[0][1].ToString());

                VendorBolNum.Text = ds.Tables[0].Rows[0][0].ToString();
                FgItmNum.Text = ds.Tables[0].Rows[0][1].ToString();
                VenJobNum.Text = ds.Tables[0].Rows[0][3].ToString();
                VenJob2Num.Text = ds.Tables[0].Rows[0][4].ToString();
                VenOrdNum.Text = ds.Tables[0].Rows[0][5].ToString();
                CustPoLineNum.Text = ds.Tables[0].Rows[0][6].ToString();
                CustPoNum.Text = ds.Tables[0].Rows[0][7].ToString();
                QtyUsed.Text = ds.Tables[0].Rows[0][8].ToString();
                CustVenCode.Text = ds.Tables[0].Rows[0][9].ToString();
                CustDptCod.Text = ds.Tables[0].Rows[0][10].ToString();
                CustPlantId.Text = ds.Tables[0].Rows[0][11].ToString();
                CustPartNum.Text = ds.Tables[0].Rows[0][12].ToString();
                CustHandQty.Text = ds.Tables[0].Rows[0][13].ToString();

                string getdate = ds.Tables[0].Rows[0][14].ToString();
                string[] val = getdate.Split(new char[] { ' ' });
                UsgDate.Text = val[0].ToString();

                ItmSelPrice.Text = ds.Tables[0].Rows[0][15].ToString();
                error.Text = "";
            }
            catch
            {
                //HttpContext.Current.Response.Write("<script>alert('Invalid Bol Number')</script>");
                error.Text = "Invalid Bol Number";
            }

        }


        protected void Tagtextbox_Click(object sneder, EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Label seq = (Label)FormView1.FindControl("Label1");
            TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
            TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
            TextBox tag = (TextBox)FormView1.FindControl("TextBox1");

            TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
            TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
            TextBox item = (TextBox)FormView1.FindControl("TextBox4");
            TextBox itemname = (TextBox)FormView1.FindControl("TextBox5");
            TextBox loc = (TextBox)FormView1.FindControl("TextBox2");
            TextBox locbin = (TextBox)FormView1.FindControl("TextBox3");
            TextBox cases = (TextBox)FormView1.FindControl("TextBox6");
            TextBox qtycas = (TextBox)FormView1.FindControl("TextBox7");
            TextBox casunit = (TextBox)FormView1.FindControl("TextBox8");
            TextBox partial = (TextBox)FormView1.FindControl("TextBox9");
            TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
            Label create = (Label)FormView1.FindControl("vCreatedByLabel");
            Label updateby = (Label)FormView1.FindControl("vCreate2Label");
            if (tag.Text.Trim() == "")
            {
                tag.Focus();
                return;
            }
            try
            {
                browspo fgtaglook = new browspo();
                DataSet ds = new DataSet();

                ds = fgtaglook.SelectfgtakLook("PoSearch", UserLogin.UserName, "Tag#", "EQUAL", tag.Text.Trim(), "no", "");

                if (ds.Tables[0].Rows.Count == 0)
                {
                    HttpContext.Current.Response.Write("<script>alert('Invalid Tag#!' )</script>");
                    tag.Focus();
                }
                else
                {
                    loc.Text = ds.Tables[0].Rows[0][7].ToString();
                    locbin.Text = ds.Tables[0].Rows[0][8].ToString();
                    job.Text = ds.Tables[0].Rows[0][5].ToString();
                    jobno2.Text = ds.Tables[0].Rows[0][6].ToString();
                    item.Text = ds.Tables[0].Rows[0][3].ToString();
                    itemname.Text = ds.Tables[0].Rows[0][4].ToString();
                    cases.Text = ds.Tables[0].Rows[0][18].ToString();
                    qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
                    casunit.Text = ds.Tables[0].Rows[0][12].ToString();
                    partial.Text = ds.Tables[0].Rows[0][24].ToString();
                    //stdcost.Text = ds.Tables[0].Rows[0][15].ToString();
                    //costuom.Text = ds.Tables[0].Rows[0][13].ToString();
                    t_qty.Text = ds.Tables[0].Rows[0][16].ToString();
                    tag.Focus();
                }
            }
            catch { }
        }





    }

}